// Copyright 2011 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package parse

import (
	"fmt"
	"strings"
	"unicode"
	"unicode/utf8"
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
	trimMarker = "-"       // Attached inside a delimiter, trims space from outside it
)

// stateFn represents the state of the scanner as a function that returns the next state.
type stateFn func(*lexer) stateFn

// lexer holds the state of the scanner.
type lexer struct {
	scanner
	name           string // the name of the input; used only for error reports
	leftDelim      string // start of action
	rightDelim     string // end of action
	trimRightDelim string // end of action with trim marker
	parenDepth     int    // nesting depth of ( ) exprs
}

// errorf emits an error token and terminates the scan by passing
// back a nil pointer that will be the next state, terminating l.nextItem.
func (l *lexer) errorf(format string, args ...interface{}) stateFn {
	l.emitItem(l.captureError(format, args...))
	return nil
}

// captureError captures a formatted error item without starting a new item
func (l *lexer) captureError(format string, args ...interface{}) *item {
	return l.captureString(itemError, fmt.Sprintf(format, args...))
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
		trimRightDelim: trimMarker + right,
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

// lexText scans until an opening action delimiter, skipping empty actions
// and doing text trimming.
func lexText(l *lexer) stateFn {
	if !l.consumeUntil(l.leftDelim) {
		// Correctly reached EOF.
		if l.haveItem() {
			l.emit(itemText)
		}
		l.emit(itemEOF)
		return nil
	}

	// We've reached a delimiter; save the text and delimiter, then do any
	// necessary trimming
	textItem := *l.capture(itemText)
	l.consumePrefix(l.leftDelim)

	// Save the item until we know we're not a comment/empty action
	delimiter := *l.capture(itemLeftDelim)

	if l.hasPrefix(trimMarker) {
		// Check for whitespace after the -, to see if it's a trim indicator
		_, w := l.next()
		if l.accept(spaceChars) {
			// trim spaces from the already-collected text
			textItem.val = strings.TrimRight(textItem.val, spaceChars)
		} else {
			l.backup(w)
		}
		l.startItem() // don't include the trim marker in next token
	}

	if textItem.val != "" {
		// No need to emit if it was trimmed away (or empty to begin with)
		l.emitItem(&textItem)
	}

	// Scan ahead to see if action is finished (i.e. whitespace or comment-only)
	if item, finished := l.scanPastWhitespace(); finished {
		// action is empty; drop the item (unless it's an error) and keep going
		if item != nil && item.typ == itemError {
			l.emitItem(item)
			return nil
		}
		return lexText
	}

	// Time to lex the action body
	l.emitItem(&delimiter)
	l.parenDepth = 0
	return lexInsideAction
}

// scanPastWhitespace scans past whitespace and comments, until it finds a
// closing action delimiter or something else, returning an item of type
// itemSpace, itemError, or itemRightDelim -- or nil if no spaces, comments,
// or delimiter are found.  The finished flag is true if an error or delimiter
// was encountered, i.e. if there isn't anything else to process in the current
// action.
//
// Errors can be generated for unclosed comments or parentheses.
// If a closing delimiter is encountered, its trim status is checked and
// whitespace following the delimeter is trimmed as needed.
//
// The item return value is borrowed from l.lastItem, so it is only valid
// until the next capture() or error.  Comments are represented as an item of
// type itemSpace with text value "".
func (l *lexer) scanPastWhitespace() (item *item, finished bool) {
	for {
		// Check for whitespace first
		l.acceptRun(spaceChars)
		if l.haveItem() {
			// Could we be at a trim delimiter?
			if l.hasPrefix(l.trimRightDelim) {
				// Yep, process it
				if l.parenDepth != 0 {
					return l.captureError("unclosed left paren"), true
				}
				l.consumePrefix(trimMarker)

				l.startItem()
				l.consumePrefix(l.rightDelim)
				item = l.capture(itemRightDelim)

				// Trim whitespace
				l.acceptRun(spaceChars)
				l.startItem()
				return item, true
			}
			// Otherwise save the token and look for comments
			item = l.capture(itemSpace)
		}

		// After whitespace, check comments
		if l.consumePrefix(leftComment) {
			if !l.consumeUntil(rightComment) {
				return l.captureError("unclosed comment"), true
			}
			l.consumePrefix(rightComment)
			l.startItem()
			item = l.capture(itemSpace) // comment = zero length space
			continue
		}

		// Finally, check plain delimiter
		if l.consumePrefix(l.rightDelim) {
			if l.parenDepth != 0 {
				return l.captureError("unclosed left paren"), true
			}
			return l.capture(itemRightDelim), true
		}

		// Either spaces, empty space, or nothing
		return item, false
	}
}

// lexInsideAction scans the elements inside action delimiters.
func lexInsideAction(l *lexer) stateFn {
	// Either number, quoted string, or identifier.
	// Spaces separate arguments; runs of spaces turn into itemSpace.
	// Pipe symbols separate and are emitted.

	// Scan for whitespace, comments, or a closing delimiter
	item, actionFinished := l.scanPastWhitespace()
	if item != nil {
		l.emitItem(item)
		if item.typ == itemError {
			return nil
		}
	}
	if actionFinished {
		return lexText
	}

	switch r, w := l.next(); {
	case r == eof:
		return l.errorf("unclosed action")
	case r == '=':
		l.emit(itemAssign)
	case r == ':':
		if !l.accept("=") {
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
		if r, _ := l.peek(); r < '0' || '9' < r {
			return lexField
		}
		fallthrough // '.' can start a number.
	case r == '+' || r == '-' || ('0' <= r && r <= '9'):
		l.backup(w)
		return lexNumber
	case isAlphaNumeric(r):
		l.backup(w)
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

// lexIdentifier scans an alphanumeric.
func lexIdentifier(l *lexer) stateFn {
Loop:
	for {
		switch r, w := l.next(); {
		case isAlphaNumeric(r):
			// absorb.
		default:
			l.backup(w)
			word := l.itemString()
			if !l.atTerminator() {
				return l.errorf("bad character %#U", r)
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
			break Loop
		}
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
	var r rune
	var w int
	for {
		r, w = l.next()
		if !isAlphaNumeric(r) {
			l.backup(w)
			break
		}
	}
	if !l.atTerminator() {
		return l.errorf("bad character %#U", r)
	}
	l.emit(typ)
	return lexInsideAction
}

// atTerminator reports whether the input is at valid termination character to
// appear after an identifier. Breaks .X.Y into two pieces. Also catches cases
// like "$x+2" not being acceptable without a space, in case we decide one
// day to implement arithmetic.
func (l *lexer) atTerminator() bool {
	r, _ := l.peek()
	if isSpace(r) || isEndOfLine(r) {
		return true
	}
	switch r {
	case eof, '.', ',', '|', ':', ')', '(':
		return true
	}
	// Does r start the delimiter? This can be ambiguous (with delim=="//", $x/2 will
	// succeed but should fail) but only in extremely rare cases caused by willfully
	// bad choice of delimiter.
	if rd, _ := utf8.DecodeRuneInString(l.rightDelim); rd == r {
		return true
	}
	return false
}

// lexChar scans a character constant. The initial quote is already
// scanned. Syntax checking is done by the parser.
func lexChar(l *lexer) stateFn {
Loop:
	for {
		switch r, _ := l.next(); r {
		case '\\':
			if r, _ := l.next(); r != eof && r != '\n' {
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
	if sign, _ := l.peek(); sign == '+' || sign == '-' {
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

func (l *lexer) scanNumber() bool {
	// Optional leading sign.
	l.accept("+-")
	// Is it hex?
	digits := "0123456789_"
	if l.accept("0") {
		// Note: Leading 0 does not mean octal in floats.
		if l.accept("xX") {
			digits = "0123456789abcdefABCDEF_"
		} else if l.accept("oO") {
			digits = "01234567_"
		} else if l.accept("bB") {
			digits = "01_"
		}
	}
	l.acceptRun(digits)
	if l.accept(".") {
		l.acceptRun(digits)
	}
	if len(digits) == 10+1 && l.accept("eE") {
		l.accept("+-")
		l.acceptRun("0123456789_")
	}
	if len(digits) == 16+6+1 && l.accept("pP") {
		l.accept("+-")
		l.acceptRun("0123456789_")
	}
	// Is it imaginary?
	l.accept("i")
	// Next thing mustn't be alphanumeric.
	if r, _ := l.peek(); isAlphaNumeric(r) {
		l.next()
		return false
	}
	return true
}

// lexQuote scans a quoted string.
func lexQuote(l *lexer) stateFn {
Loop:
	for {
		switch r, _ := l.next(); r {
		case '\\':
			if r, _ = l.next(); r != eof && r != '\n' {
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
	if !l.consumeUntil("`") {
		return l.errorf("unterminated raw quoted string")
	}
	l.consumePrefix("`")
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
