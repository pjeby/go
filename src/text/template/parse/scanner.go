// Copyright 2011 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package parse

/*
The scanner implemented in this file is written based on the assumption
that most scanning operations (dispatch on a character, match character
classes, etc.) will be targeting ASCII characters, while still supporting
full utf8 as input.  But because ASCII characters only appear in utf8 to
represent themselves, and the scanner's output is always a slice of the input,
some significant optimizations are possible.

Specifically, whenever the scanner is asked to look for something in ASCII
(i.e. via accept(), acceptAny(), and acceptEither()), it can avoid utf8
decoding and just compare bytes.  If the byte isn't ASCII, it's not going to
match anyway, so the cursor stays put at a valid position.

If, on the other hand, a check to match something non-ASCII is needed, one
can use peek() to force decoding of the current rune before trying to move past
it, or use hasPrefix() to safely compare against another utf8-encoded string.
(Or acceptUntil() to skip ahead to a known-valid, position-independent delimiter
string.)

The only other requirement to avoid an invalid scan position is that any
position manipulation be done with known-valid byte lengths.  For backup() this
is easy: just use the length returned by next().  For advanceBy(), use the
length of a utf8-encoded string you've already matched, or a known character
width.  And only call next() if you've just called peek(), know the current rune
is ASCII, or will be looping past any non-ASCII characters searching for
specific ASCII character(s).

Following these rules allows a significant performance increase, as it allows
the scanner to avoid utf8 decoding at the majority of input positions, either
because they're ASCII, or going to be compared to ASCII.  In addition, the
thus-simplified scanner methods can almost all be inlined back into the lexer,
speeding things up even further.  (This requires that neither peek() nor next()
call each other, lest they get too big to inline.)

Finally, to support fast character-class testing, a scanset() function is
supplied that converts an ASCII string to a *[128]bool mask with a contains(rune)
method.  This method returns false if the rune is EOF, non-ASCII, or not in the
string that was used to create the mask.  (The scanner's acceptAny(mask) method
is a faster form of mask.contains(scanner.peek()), that simply compares against
the current byte without needing to decode it first, calling next() if it's a
match...  which is safe because a match means it's ASCII.)
*/

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

// A mask of ASCII characters to match for acceptRun or acceptAny
type scan_mask [128]bool

// contains returns true if the mask contains the given rune
func (mask *scan_mask) contains(r rune) bool {
	return (r & ^127) == 0 && mask[r]
}

// Convert a string to a character mask; will panic if given non-ASCII runes!
func scanset(s string) (mask *scan_mask) {
	mask = new(scan_mask)
	for _, r := range s {
		mask[r] = true
	}
	return
}

// Sentinel for end of input
const eof = -1

type scanner struct {
	input     string // the string being scanned
	pos       Pos    // current position in the input
	end       Pos    // position after the end of input
	start     Pos    // start position of this item
	startLine int    // start line of this item
	nextRune  rune   // current lookahead rune
	nextWidth int    // current lookahead width
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

// peek returns (but does not consume) the next rune in the input.
func (s *scanner) peek() rune {
	// only decode if needed; eof is -1, so will not decode
	if s.nextRune > unicode.MaxASCII {
		s.nextRune, s.nextWidth = utf8.DecodeRuneInString(s.input[s.pos:])
	}
	return s.nextRune
}

// next advances the input by one rune, returning the size of that rune in
// bytes so you can backup() if needed.  (May advance into the middle of a
// utf8-encoded character if you haven't called peek() since the last change
// in cursor position.)
func (s *scanner) next() (w int) {
	w = s.nextWidth
	s.advanceBy(w)
	return
}

// backup steps back by the specified number of bytes; may result in an invalid
// position unless you know exactly what you're backing up over.
func (s *scanner) backup(w int) {
	s.advanceBy(-w)
}

// itemString returns the string of input accepted since the last startNewItem
func (s *scanner) itemString() string {
	return s.input[s.start:s.pos]
}

// capture creates an item from itemString() and starts a new one
func (s *scanner) capture(t itemType) (i item) {
	i = item{t, s.start, s.itemString(), s.startLine}
	s.startNewItem()
	return
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
// current scanning position (such as the len() of something matched with
// hasPrefix(), or the length of the delimiter after using acceptUntil()).
func (s *scanner) advanceBy(width int) {
	s.pos += Pos(width)
	if s.pos >= s.end {
		s.nextRune = eof
		s.nextWidth = 0
	} else {
		// optimistically assume next character is ASCII.  If we're wrong,
		// peek() will correct it
		s.nextRune = rune(s.input[s.pos])
		s.nextWidth = 1
	}
	return
}

// accept returns true and consumes the next input rune if it matches the
// supplied rune.  The supplied rune MUST be ASCII, unless peek() is called
// first.
func (s *scanner) accept(r rune) bool {
	if s.nextRune == r {
		s.advanceBy(s.nextWidth)
		return true
	}
	return false
}

// acceptEither returns true and consumes the next input rune if it matches
// either of the supplied runes.  The supplied runes MUST be ASCII, unless peek()
// is called first.
func (s *scanner) acceptEither(r1 rune, r2 rune) bool {
	if s.nextRune == r1 || s.nextRune == r2 {
		s.advanceBy(s.nextWidth)
		return true
	}
	return false
}

// acceptAny returns true and consumes the next input rune if it's in the given
// scanset() mask.
func (s *scanner) acceptAny(mask *scan_mask) (found bool) {
	if found = mask.contains(s.nextRune); found {
		s.advanceBy(1) // mask can only match ASCII runes
	}
	return
}

// hasPrefix returns true if the given string is a prefix of the remaining input
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
