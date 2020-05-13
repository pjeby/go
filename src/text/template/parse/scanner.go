// Copyright 2011 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package parse

import (
	"fmt"
	"strings"
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

// A mask of ASCII characters to match for acceptRun or acceptAny
type scan_mask [128]bool

// Convert a string to a character mask; will panic if given non-ASCII runes!
func scanset(s string) (mask *scan_mask) {
	mask = new(scan_mask)
	for _, r := range s { mask[r] = true }
	return
}

// Sentinel for end of input
const eof = -1

type scanner struct {
	input     string                 // the string being scanned
	pos       Pos                    // current position in the input
	end       Pos                    // position after the end of input
	start     Pos                    // start position of this item
	startLine int                    // start line of this item
	lastItem  item                   // last item capture()d
	nextRune  rune                   // current lookahead rune
	nextWidth int                    // current lookahead width
}

// scan creates a new scanner for the input string.
func scan(input string) scanner {
	s := &scanner{
		input:     input,
		end:       Pos(len(input)),
		startLine: 1,
	}
	s.advanceBy(0) // initialize the lookahead rune
	return *s
}

// peek returns but does not consume the next rune in the input
func (s *scanner) peek() rune {
	return s.nextRune
}

// next returns the next rune in the input and advances past it.
func (s *scanner) next() (r rune, w int) {
	r = s.nextRune
	w = s.nextWidth
	s.advanceBy(w)
	return
}

// backup steps back to a previous rune+width returned by next().  No error
// checking is performed: you are responsible for passing in the right r/w pair.
func (s *scanner) backup(r rune, w int) {
	s.pos -= Pos(w)
	s.nextRune = r
	s.nextWidth = w
}

// itemString returns the string that would be emitted for an item, if you
// emitted it now.
func (s *scanner) itemString() string {
	return s.input[s.start:s.pos]
}

// capture creates an item from the current lex state and starts a new one.
// The return value is a pointer to within the struct, so you must copy it
// if you want to use the item past the next capture operation.
func (s *scanner) capture(t itemType) (i *item) {
	s.lastItem = item{t, s.start, s.itemString(), s.startLine}
	s.startNewItem()
	return &s.lastItem
}

// captureString captures an explicit string in place of s.itemString()
func (s *scanner) captureString(typ itemType, text string) *item {
	s.lastItem = item{typ, s.start, text, s.startLine}
	return &s.lastItem
}

// startNewItem marks the beginning of a new item to be captured later
func (s *scanner) startNewItem() {
	s.startLine += strings.Count(s.itemString(), "\n")
	s.start = s.pos
}

// haveItem is true if the position has advanced since the last new item start
func (s *scanner) haveItem() bool {
	return s.pos > s.start
}

// advanceBy skips the specified number of bytes in the input; it should only
// be used with a value known to match a valid prefix of the input at the
// current scanning position (e.g. the len() of something matched with
// hasPrefix(), or the length of the delimiter after a successful acceptUntil.
func (s *scanner) advanceBy(width int) {
	s.pos += Pos(width)
	if s.pos >= s.end {
		s.nextRune = eof
		s.nextWidth = 0
	} else {
		s.nextRune, s.nextWidth = utf8.DecodeRuneInString(s.input[s.pos:])
	}
	return
}

// accept consumes the next rune if it matches the supplied one; return value
// is true if it did so.
func (s *scanner) accept(r rune) bool {
	if s.nextRune == r {
		s.advanceBy(s.nextWidth)
		return true
	}
	return false
}

// accept consumes the next rune if it matches either of the supplied ones
func (s *scanner) acceptEither(r1 rune, r2 rune) bool {
	if s.nextRune == r1 || s.nextRune == r2 {
		s.advanceBy(s.nextWidth)
		return true
	}
	return false
}

// acceptAny consumes the next rune if it's in the given mask.
func (s *scanner) acceptAny(mask *scan_mask) (found bool) {
	if found = (s.nextRune & ^127) == 0 && mask[s.nextRune]; found {
		s.advanceBy(s.nextWidth)
	}
	return
}

// acceptRun consumes a series of zero or more runes matching the given mask.
func (s *scanner) acceptRun(mask *scan_mask) {
	for s.acceptAny(mask) {
	}
}

// hasPrefix returns true if the input prefix matches the string at the current
// position
func (s *scanner) hasPrefix(prefix string) bool {
	return strings.HasPrefix(s.input[s.pos:], prefix)
}

// acceptUntil consumes input until the scanner hasPrefix(delimiter) or EOF.
// The return is true if the delimiter is found, false if EOF was reached first.
// In either case, the delimiter itself has not been consumed.
func (s *scanner) acceptUntil(delimiter string) (found bool) {
	if x := strings.Index(s.input[s.pos:], delimiter); x >= 0 {
		found = true
		s.advanceBy(x)
	} else {
		// Jump to EOF
		s.pos = s.end
		s.advanceBy(0)
	}
	return
}
