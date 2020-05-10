// Copyright 2011 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package parse

import (
	"fmt"
	"strings"
	"unicode"
)

const (
	itemError        itemType = iota // error occurred; value is text of error
	itemBool                         // boolean constant
	itemChar                         // printable ASCII character; grab bag for comma etc.
	itemCharConstant                 // character constant
	itemComplex                      // complex constant (1+2i); imaginary is just a number
	itemAssign                       // equals ('=') introducing an assignment
	itemDeclare                      // colon-equals (':=') introducing a declaration
	itemEOF
	itemField      // alphanumeric identifier starting with '.'
	itemIdentifier // alphanumeric identifier not starting with '.'
	itemLeftDelim  // left action delimiter
	itemLeftParen  // '(' inside action
	itemNumber     // simple number, including imaginary
	itemPipe       // pipe symbol
	itemRawString  // raw quoted string (includes quotes)
	itemRightDelim // right action delimiter
	itemRightParen // ')' inside action
	itemSpace      // run of spaces separating arguments
	itemString     // quoted string (includes quotes)
	itemText       // plain text
	itemVariable   // variable starting with '$', such as '$' or  '$1' or '$hello'
	// Keywords appear after all the rest.
	itemKeyword  // used only to delimit the keywords
	itemBlock    // block keyword
	itemDot      // the cursor, spelled '.'
	itemDefine   // define keyword
	itemElse     // else keyword
	itemEnd      // end keyword
	itemIf       // if keyword
	itemNil      // the untyped nil constant, easiest to treat as a keyword
	itemRange    // range keyword
	itemTemplate // template keyword
	itemWith     // with keyword
)

var key = map[string]itemType{
	".":        itemDot,
	"block":    itemBlock,
	"define":   itemDefine,
	"else":     itemElse,
	"end":      itemEnd,
	"if":       itemIf,
	"range":    itemRange,
	"nil":      itemNil,
	"template": itemTemplate,
	"with":     itemWith,
}

// Trimming spaces.
// If the action begins "{{- " rather than "{{", then all space/tab/newlines
// preceding the action are trimmed; conversely if it ends " -}}" the
// leading spaces are trimmed. This is done entirely in the lexer; the
// parser never sees it happen. We require whitespace to be
// present to avoid ambiguity with things like "{{-3}}". It reads
// better with whitespace present anyway.
const (
	spaceChars = " \t\r\n" // These are the space characters defined by Go itself.
	trimMarker = '-'       // Attached inside a delimiter, trims space from outside it
)

var whitespace = scanset(spaceChars)

// stateFn represents the state of the scanner as a function that returns the next state.
type stateFn func(*lexer) stateFn

// lexer holds the state of the scanner.
type lexer struct {
	scanner
	name           string    // the name of the input; used only for error reports
	leftDelim      string    // start of action
	rightDelim     string    // end of action
	trimRightDelim string    // end of action with trim marker
	items          chan item // channel of scanned items
	parenDepth     int       // nesting depth of ( ) exprs
}

// emit captures an item and passes it back to the client.
func (l *lexer) emit(t itemType) {
	l.emitItem(l.capture(t))
}

// emitItem passes a specified item back to the client.
func (l *lexer) emitItem(i item) {
	l.items <- i
}

// errorf emits an error token and terminates the scan by passing
// back a nil pointer that will be the next state, terminating l.nextItem.
func (l *lexer) errorf(format string, args ...interface{}) stateFn {
	i := l.capture(itemError)
	i.val = fmt.Sprintf(format, args...)
	l.emitItem(i)
	return nil
}

// nextItem returns the next item from the input.
// Called by the parser, not in the lexing goroutine.
func (l *lexer) nextItem() item {
	return <-l.items
}

// drain drains the output so the lexing goroutine will exit.
// Called by the parser, not in the lexing goroutine.
func (l *lexer) drain() {
	for range l.items {
	}
}

// lex creates a new scanner for the input string.
func lex(name, input, left, right string) *lexer {
	if left == "" {
		left = leftDelim
	}
	if right == "" {
		right = rightDelim
	}
	l := &lexer{
		name:           name,
		leftDelim:      left,
		rightDelim:     right,
		trimRightDelim: string(trimMarker) + right,
		items:          make(chan item),
		scanner:        scan(input),
	}
	go l.run()
	return l
}

// run runs the state machine for the lexer.
func (l *lexer) run() {
	for state := lexText; state != nil; {
		state = state(l)
	}
	close(l.items)
}

// state functions

const (
	leftDelim    = "{{"
	rightDelim   = "}}"
	leftComment  = "/*"
	rightComment = "*/"
)

// lexText scans until an opening action delimiter, trimming whitespace from
// the preceding text if applicable.
func lexText(l *lexer) stateFn {
	if !l.acceptUntil(l.leftDelim) {
		// Correctly reached EOF.
		if l.haveItem() {
			l.emit(itemText)
		}
		l.emit(itemEOF)
		return nil
	}

	// We've reached a delimiter; save the text and delimiter, then do any
	// necessary trimming
	textItem := l.capture(itemText)
	l.advanceBy(len(l.leftDelim))

	// Save the delimiter item until we've handled the trimming
	delimiter := l.capture(itemLeftDelim)

	if l.peek() == trimMarker {
		// Check for whitespace after the -, to see if it's a trim indicator
		w := l.next()
		if l.acceptAny(whitespace) {
			// trim spaces from the already-collected text
			textItem.val = strings.TrimRight(textItem.val, spaceChars)
		} else {
			l.backup(w)
		}
		l.startNewItem() // don't include the trim marker in next token
	}

	if len(textItem.val) > 0 {
		// No need to emit if it was trimmed away (or empty to begin with)
		l.emitItem(textItem)
	}

	// Time to lex the action body
	l.emitItem(delimiter)
	l.parenDepth = 0
	return lexInsideAction
}

// lexInsideAction scans the elements inside action delimiters.
func lexInsideAction(l *lexer) stateFn {
	// Either number, quoted string, or identifier.
	// Spaces separate arguments; runs of spaces turn into itemSpace.
	// Pipe symbols separate and are emitted.
	if l.hasPrefix(l.rightDelim) {
		l.advanceBy(len(l.rightDelim))
		if l.parenDepth == 0 {
			l.emit(itemRightDelim)
			return lexText
		}
		return l.errorf("unclosed left paren")
	}
	switch r, w := l.peek(), l.next(); {
	case r == eof:
		return l.errorf("unclosed action")
	case isSpace(r) || isEndOfLine(r) || (r == '/' && l.peek() == '*'):
		l.backup(w) // Put space back in case we have " -}}" (or / for "/*").
		return lexSpace
	case r == '=':
		l.emit(itemAssign)
	case r == ':':
		if !l.accept('=') {
			return l.errorf("expected :=")
		}
		l.emit(itemDeclare)
	case r == '|':
		l.emit(itemPipe)
	case r == '"':
		return lexQuote
	case r == '`':
		return lexRawQuote
	case r == '$':
		return lexVariable
	case r == '\'':
		return lexChar
	case r == '.':
		// look-ahead for ".field"
		if r := l.peek(); r < '0' || '9' < r {
			return lexField
		}
		fallthrough // '.' can start a number.
	case r == '+' || r == '-' || ('0' <= r && r <= '9'):
		l.backup(w)
		return lexNumber
	case isAlphaNumeric(r):
		return lexIdentifier
	case r == '(':
		l.emit(itemLeftParen)
		l.parenDepth++
	case r == ')':
		l.emit(itemRightParen)
		l.parenDepth--
		if l.parenDepth < 0 {
			return l.errorf("unexpected right paren %#U", r)
		}
	case r <= unicode.MaxASCII && unicode.IsPrint(r):
		l.emit(itemChar)
	default:
		return l.errorf("unrecognized character in action: %#U", r)
	}
	return lexInsideAction
}

// lexSpace scans a run of spaces or comments.  We are assumed to be placed
// at a whitespace character or comment opener, unconsumed.  Because a closing
// trim marker can *only* occur after whitespace, this is the only state where
// its existence needs to be checked for.
//
// Note: at most one item is emitted from this state: either a space item for
// the most recent run of spaces, an empty space item for the most recent comment,
// an error (for an unclosed comment), or a delimiter (for a trim delimiter).
func lexSpace(l *lexer) stateFn {
	var item item
	var found bool
	for {
		if l.acceptAny(whitespace) {
			for l.acceptAny(whitespace) {
			}
			// Could we be at a trim delimiter?
			if l.hasPrefix(l.trimRightDelim) {
				return lexRightTrimDelimiter
			}
			// Nope, save the token and look for comments
			item = l.capture(itemSpace)
			found = true
			// fall through to comment check
		}
		if l.hasPrefix(leftComment) {
			l.advanceBy(len(leftComment))
			if !l.acceptUntil(rightComment) {
				return l.errorf("unclosed comment")
			}
			l.advanceBy(len(rightComment))
			l.startNewItem()
			item = l.capture(itemSpace) // comment = zero length space
			found = true
			continue // loop and look for whitespace
		}
		// No more comments or whitespace
		if found {
			// But we got something before this point, so give it up
			l.emitItem(item)
		}
		return lexInsideAction
	}
}

// lexRightTrimDelimiter processes the closing of an action with right-side
// whitespace trimming.  It is invoked with the scan position at the trim
// delimiter (i.e. "-" + the closing delimiter string, since the whitespace
// was processed by lexSpace).
func lexRightTrimDelimiter(l *lexer) stateFn {
	if l.parenDepth != 0 {
		return l.errorf("unclosed left paren")
	}
	l.accept(trimMarker)
	l.startNewItem()
	l.advanceBy(len(l.rightDelim))
	l.emit(itemRightDelim)

	// Trim whitespace from non-action text
	for l.acceptAny(whitespace) {
	}
	l.startNewItem()
	return lexText
}

// lexIdentifier scans an alphanumeric.
func lexIdentifier(l *lexer) stateFn {
	for isAlphaNumeric(l.peek()) {
		l.next()
	}
	word := l.itemString()
	if !l.atTerminator() {
		return l.errorf("bad character %#U", l.peek())
	}
	switch {
	case key[word] > itemKeyword:
		l.emit(key[word])
	case word[0] == '.':
		l.emit(itemField)
	case word == "true", word == "false":
		l.emit(itemBool)
	default:
		l.emit(itemIdentifier)
	}
	return lexInsideAction
}

// lexField scans a field: .Alphanumeric.
// The . has been scanned.
func lexField(l *lexer) stateFn {
	return lexFieldOrVariable(l, itemField)
}

// lexVariable scans a Variable: $Alphanumeric.
// The $ has been scanned.
func lexVariable(l *lexer) stateFn {
	if l.atTerminator() { // Nothing interesting follows -> "$".
		l.emit(itemVariable)
		return lexInsideAction
	}
	return lexFieldOrVariable(l, itemVariable)
}

// lexVariable scans a field or variable: [.$]Alphanumeric.
// The . or $ has been scanned.
func lexFieldOrVariable(l *lexer, typ itemType) stateFn {
	if l.atTerminator() { // Nothing interesting follows -> "." or "$".
		if typ == itemVariable {
			l.emit(itemVariable)
		} else {
			l.emit(itemDot)
		}
		return lexInsideAction
	}
	for isAlphaNumeric(l.peek()) {
		l.next()
	}
	if !l.atTerminator() {
		return l.errorf("bad character %#U", l.peek())
	}
	l.emit(typ)
	return lexInsideAction
}

var terminators = scanset(spaceChars + ".,|:)(")

// atTerminator reports whether the input is at valid termination character to
// appear after an identifier. Breaks .X.Y into two pieces. Also catches cases
// like "$x+2" not being acceptable without a space, in case we decide one
// day to implement arithmetic.
func (l *lexer) atTerminator() bool {
	if l.acceptAny(terminators) {
		l.backup(1) // always ASCII
		return true
	}
	// Only other valid cases are eof, comment, or closing delimiter; otherwise false
	return l.peek() == eof || l.hasPrefix(leftComment) || l.hasPrefix(l.rightDelim)
}

// lexChar scans a character constant. The initial quote is already
// scanned. Syntax checking is done by the parser.
func lexChar(l *lexer) stateFn {
Loop:
	for {
		switch r, _ := l.peek(), l.next(); r {
		case '\\':
			if r, _ := l.peek(), l.next(); r != eof && r != '\n' {
				break
			}
			fallthrough
		case eof, '\n':
			return l.errorf("unterminated character constant")
		case '\'':
			break Loop
		}
	}
	l.emit(itemCharConstant)
	return lexInsideAction
}

// lexNumber scans a number: decimal, octal, hex, float, or imaginary. This
// isn't a perfect number scanner - for instance it accepts "." and "0x0.2"
// and "089" - but when it's wrong the input is invalid and the parser (via
// strconv) will notice.
func lexNumber(l *lexer) stateFn {
	if !l.scanNumber() {
		return l.errorf("bad number syntax: %q", l.itemString())
	}
	if sign := l.peek(); sign == '+' || sign == '-' {
		// Complex: 1+2i. No spaces, must end in 'i'.
		if !l.scanNumber() || !strings.HasSuffix(l.itemString(), "i") {
			return l.errorf("bad number syntax: %q", l.itemString())
		}
		l.emit(itemComplex)
	} else {
		l.emit(itemNumber)
	}
	return lexInsideAction
}

var bases = [17]*scan_mask{
	10: scanset("0123456789_"),
	16: scanset("0123456789abcdefABCDEF_"),
	8:  scanset("01234567_"),
	2:  scanset("01_"),
}

func (l *lexer) scanNumber() bool {
	// Optional leading sign.
	l.acceptEither('+', '-')
	// Is it hex?
	base := 10
	if l.accept('0') {
		// Note: Leading 0 does not mean octal in floats.
		if l.acceptEither('x', 'X') {
			base = 16
		} else if l.acceptEither('o', 'O') {
			base = 8
		} else if l.acceptEither('b', 'B') {
			base = 2
		}
	}
	digits := bases[base]
	for l.acceptAny(digits) {
	}
	if l.accept('.') {
		for l.acceptAny(digits) {
		}
	}
	if base == 10 && l.acceptEither('e', 'E') {
		l.acceptEither('+', '-')
		for l.acceptAny(digits) {
		}
	}
	if base == 16 && l.acceptEither('p', 'P') {
		l.acceptEither('+', '-')
		for l.acceptAny(bases[10]) {
		}
	}
	// Is it imaginary?
	l.accept('i')
	// Next thing mustn't be alphanumeric.
	if isAlphaNumeric(l.peek()) {
		l.next()
		return false
	}
	return true
}

// lexQuote scans a quoted string.
func lexQuote(l *lexer) stateFn {
Loop:
	for {
		switch r, _ := l.peek(), l.next(); r {
		case '\\':
			if r, _ = l.peek(), l.next(); r != eof && r != '\n' {
				break
			}
			fallthrough
		case eof, '\n':
			return l.errorf("unterminated quoted string")
		case '"':
			break Loop
		}
	}
	l.emit(itemString)
	return lexInsideAction
}

// lexRawQuote scans a raw quoted string.
func lexRawQuote(l *lexer) stateFn {
	if !l.acceptUntil("`") {
		return l.errorf("unterminated raw quoted string")
	}
	l.accept('`')
	l.emit(itemRawString)
	return lexInsideAction
}

// isSpace reports whether r is a space character.
func isSpace(r rune) bool {
	return r == ' ' || r == '\t'
}

// isEndOfLine reports whether r is an end-of-line character.
func isEndOfLine(r rune) bool {
	return r == '\r' || r == '\n'
}

// isAlphaNumeric reports whether r is an alphabetic, digit, or underscore.
func isAlphaNumeric(r rune) bool {
	return r == '_' || unicode.IsLetter(r) || unicode.IsDigit(r)
}
