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

// item represents a token or text string returned from the scanner.
type item struct {
	typ  itemType // The type of this item.
	pos  Pos      // The starting position, in bytes, of this item in the input string.
	val  string   // The value of this item.
	line int      // The line number at the start of this item.
}

func (i item) String() string {
	switch {
	case i.typ == itemEOF:
		return "EOF"
	case i.typ == itemError:
		return i.val
	case i.typ > itemKeyword:
		return fmt.Sprintf("<%s>", i.val)
	case len(i.val) > 10:
		return fmt.Sprintf("%.10q...", i.val)
	}
	return fmt.Sprintf("%q", i.val)
}

// itemType identifies the type of lex items.
type itemType int

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

const eof = -1

// Trimming spaces.
// If the action begins "{{- " rather than "{{", then all space/tab/newlines
// preceding the action are trimmed; conversely if it ends " -}}" the
// leading spaces are trimmed. This is done entirely in the lexer; the
// parser never sees it happen. We require an ASCII space to be
// present to avoid ambiguity with things like "{{-3}}". It reads
// better with the space present anyway. For simplicity, only ASCII
// space does the job.
const (
	spaceChars      = " \t\r\n" // These are the space characters defined by Go itself.
	leftTrimMarker  = "- "      // Attached to left delimiter, trims trailing spaces from preceding text.
	rightTrimMarker = " -"      // Attached to right delimiter, trims leading spaces from following text.
	trimMarkerLen   = Pos(len(leftTrimMarker))
)

// stateFn represents the state of the scanner as a function that returns the next state.
type stateFn func(*lexer) stateFn

// lexer holds the state of the scanner.
type lexer struct {
	name           string    // the name of the input; used only for error reports
	input          string    // the string being scanned
	leftDelim      string    // start of action
	rightDelim     string    // end of action
	trimRightDelim string    // end of action with trim marker
	pos            Pos       // current position in the input
	end            Pos       // position after the end of input
	start          Pos       // start position of this item
	items          chan item // channel of scanned items
	parenDepth     int       // nesting depth of ( ) exprs
	startLine      int       // start line of this item
	lastItem       item      // last item capture()d or emit()ted
}

// peek returns but does not consume the next rune in the input (and its width)
// it's kept simple so it can be inlined into next()
func (l *lexer) peek() (r rune, w int) {
	if l.pos >= l.end {
		r = eof
	} else {
		r, w = utf8.DecodeRuneInString(l.input[l.pos:])
	}
	return
}

// next returns the next rune in the input.
func (l *lexer) next() (r rune, w int) {
	r, w = l.peek()
	l.advanceBy(w)
	return
}

// backup steps back a known-valid width
func (l *lexer) backup(width int) {
	l.pos -= Pos(width)
}

// emit captures an item and passes it back to the client.
func (l *lexer) emit(t itemType) {
	l.emitItem(l.capture(t))
}

// emitItem passes a specified item back to the client.
func (l *lexer) emitItem(i *item) {
	l.items <- *i
}

// itemString returns the string that would be emitted for an item
func (l *lexer) itemString() string {
	return l.input[l.start:l.pos]
}

// capture creates an item from the current lex state and starts a new one
func (l *lexer) capture(t itemType) (i *item) {
	l.lastItem = item{t, l.start, l.itemString(), l.startLine}
	l.startItem()
	return &l.lastItem
}

// startItem marks the beginning of a new item
func (l *lexer) startItem() {
	l.startLine += strings.Count(l.itemString(), "\n")
	l.start = l.pos
}

// haveItem is true if the position has advanced since the last new item
func (l *lexer) haveItem() bool {
	return l.pos > l.start
}

// advanceBy skips the specified number of bytes in the input
func (l *lexer) advanceBy(width int) {
	l.pos += Pos(width)
}

// accept consumes the next rune if it's from the valid set.
func (l *lexer) accept(valid string) (b bool) {
	r, w := l.peek()
	b = strings.ContainsRune(valid, r)
	if b {
		l.pos += Pos(w)
	}
	return
}

// acceptRun consumes a run of runes from the valid set.
func (l *lexer) acceptRun(valid string) {
	for l.accept(valid) {
	}
}

// hasPrefix returns true if the input prefix matches the string
func (l *lexer) hasPrefix(prefix string) bool {
	return strings.HasPrefix(l.input[l.pos:], prefix)
}

// consumePrefix consumes the prefix at the present location
func (l *lexer) consumePrefix(prefix string) (b bool) {
	b = l.hasPrefix(prefix)
	if b {
		l.advanceBy(len(prefix))
	}
	return
}

// consumeUntil consumes input until delimiter is prefix (returns true) or EOF
// (false). In either case, the input cannot be backed up, but the delimiter
// has not been consumed, and the line count has been updated
func (l *lexer) consumeUntil(delimiter string) (found bool) {
	if x := strings.Index(l.input[l.pos:], delimiter); x >= 0 {
		found = true
		l.advanceBy(x)
	} else {
		// Jump to EOF
		l.advanceBy(int(l.end - l.pos))
	}
	return
}

// errorf returns an error token and terminates the scan by passing
// back a nil pointer that will be the next state, terminating l.nextItem.
func (l *lexer) errorf(format string, args ...interface{}) stateFn {
	l.items <- item{itemError, l.start, fmt.Sprintf(format, args...), l.startLine}
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
		input:          input,
		end:            Pos(len(input)),
		leftDelim:      left,
		rightDelim:     right,
		trimRightDelim: rightTrimMarker + right,
		items:          make(chan item),
		startLine:      1,
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

// lexText scans until an opening action delimiter, "{{".
func lexText(l *lexer) stateFn {
	if l.consumeUntil(l.leftDelim) {
		textItem := *l.capture(itemText)
		if l.hasPrefix(l.leftDelim + leftTrimMarker) {
			textItem.val = strings.TrimRight(textItem.val, spaceChars)
		}
		if textItem.val != "" {
			l.emitItem(&textItem)
		}
		return lexLeftDelim
	}
	// Correctly reached EOF.
	if l.haveItem() {
		l.emit(itemText)
	}
	l.emit(itemEOF)
	return nil
}

// lexLeftDelim scans after the left delimiter for a comment
func lexLeftDelim(l *lexer) stateFn {
	l.consumePrefix(l.leftDelim)

	// save the token for later use if it's not a comment
	delimiter := *l.capture(itemLeftDelim)

	// remove the trim marker if it's there, then start a new item
	l.consumePrefix(leftTrimMarker)
	l.startItem()

	if l.hasPrefix(leftComment) {
		return lexComment
	}

	// Time to lex the action body
	l.emitItem(&delimiter)
	l.parenDepth = 0
	return lexInsideAction
}

// lexComment scans a comment. The left comment marker is known to be present.
func lexComment(l *lexer) stateFn {
	l.consumePrefix(leftComment)
	if !l.consumeUntil(rightComment) {
		return l.errorf("unclosed comment")
	}
	l.consumePrefix(rightComment)
	trimSpace := l.consumePrefix(rightTrimMarker)
	if !l.consumePrefix(l.rightDelim) {
		return l.errorf("comment ends before closing delimiter")
	}
	if trimSpace {
		l.acceptRun(spaceChars)
	}
	l.startItem()
	return lexText
}

// lexRightDelim scans the right delimiter, which is known to be present, possibly with a trim marker.
func lexRightDelim(l *lexer) stateFn {
	trimSpace := l.consumePrefix(rightTrimMarker)
	l.startItem()
	l.consumePrefix(l.rightDelim)
	l.emit(itemRightDelim)
	if trimSpace {
		l.acceptRun(spaceChars)
		l.startItem()
	}
	return lexText
}

// lexInsideAction scans the elements inside action delimiters.
func lexInsideAction(l *lexer) stateFn {
	// Either number, quoted string, or identifier.
	// Spaces separate arguments; runs of spaces turn into itemSpace.
	// Pipe symbols separate and are emitted.
	if l.hasPrefix(l.trimRightDelim) || l.hasPrefix(l.rightDelim) {
		if l.parenDepth == 0 {
			return lexRightDelim
		}
		return l.errorf("unclosed left paren")
	}
	switch r, w := l.next(); {
	case r == eof || isEndOfLine(r):
		return l.errorf("unclosed action")
	case isSpace(r):
		l.backup(w) // Put space back in case we have " -}}".
		return lexSpace
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

// lexSpace scans a run of space characters.
// We have not consumed the first space, which is known to be present.
// Take care if there is a trim-marked right delimiter, which starts with a space.
func lexSpace(l *lexer) stateFn {
	var r rune
	var w int
	var numSpaces int
	for {
		r, _ = l.peek()
		if !isSpace(r) {
			break
		}
		r, w = l.next()
		numSpaces++
	}
	// Be careful about a trim-marked closing delimiter, which has a minus
	// after a space. We know there is a space, so check for the '-' that might follow.
	l.backup(w)
	if l.hasPrefix(l.trimRightDelim) {
		if numSpaces == 1 {
			return lexRightDelim // On the delim, so go right to that.
		}
	} else {
		l.next()
	}
	// Emit the spaces
	l.emit(itemSpace)
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
		if !l.scanNumber() || l.input[l.pos-1] != 'i' {
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
