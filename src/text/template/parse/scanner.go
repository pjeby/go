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

const eof = -1

type scanner struct {
	input     string    // the string being scanned
	pos       Pos       // current position in the input
	end       Pos       // position after the end of input
	start     Pos       // start position of this item
	items     chan item // channel of scanned items
	startLine int       // start line of this item
	lastItem  item      // last item capture()d or emit()ted
}

// scan creates a new scanner for the input string.
func scan(input string) scanner {
	s := &scanner{
		input:     input,
		end:       Pos(len(input)),
		items:     make(chan item),
		startLine: 1,
	}
	return *s
}

// peek returns but does not consume the next rune in the input (and its width)
// it's kept simple so it can be inlined into next()
func (s *scanner) peek() (r rune, w int) {
	if s.pos >= s.end {
		r = eof
	} else {
		r, w = utf8.DecodeRuneInString(s.input[s.pos:])
	}
	return
}

// next returns the next rune in the input.
func (s *scanner) next() (r rune, w int) {
	r, w = s.peek()
	s.advanceBy(w)
	return
}

// backup steps back a known-valid width
func (s *scanner) backup(width int) {
	s.pos -= Pos(width)
}

// emit captures an item and passes it back to the client.
func (s *scanner) emit(t itemType) {
	s.emitItem(s.capture(t))
}

// emitItem passes a specified item back to the client.
func (s *scanner) emitItem(i *item) {
	s.items <- *i
}

// itemString returns the string that would be emitted for an item
func (s *scanner) itemString() string {
	return s.input[s.start:s.pos]
}

// capture creates an item from the current lex state and starts a new one
func (s *scanner) capture(t itemType) (i *item) {
	s.lastItem = item{t, s.start, s.itemString(), s.startLine}
	s.startItem()
	return &s.lastItem
}

// captureString captures an explicit string in place of s.itemString()
func (s *scanner) captureString(typ itemType, text string) *item {
	s.lastItem = item{typ, s.start, text, s.startLine}
	return &s.lastItem
}

// startItem marks the beginning of a new item
func (s *scanner) startItem() {
	s.startLine += strings.Count(s.itemString(), "\n")
	s.start = s.pos
}

// haveItem is true if the position has advanced since the last new item
func (s *scanner) haveItem() bool {
	return s.pos > s.start
}

// advanceBy skips the specified number of bytes in the input
func (s *scanner) advanceBy(width int) {
	s.pos += Pos(width)
}

// accept consumes the next rune if it's from the valid set.
func (s *scanner) accept(valid string) (found bool) {
	r, w := s.peek()
	if found = strings.ContainsRune(valid, r); found {
		s.advanceBy(w)
	}
	return
}

// acceptRun consumes a run of runes from the valid set.
func (s *scanner) acceptRun(valid string) {
	for s.accept(valid) {
	}
}

// hasPrefix returns true if the input prefix matches the string
func (s *scanner) hasPrefix(prefix string) bool {
	return strings.HasPrefix(s.input[s.pos:], prefix)
}

// consumePrefix consumes the prefix at the present location
func (s *scanner) consumePrefix(prefix string) (found bool) {
	if found = s.hasPrefix(prefix); found {
		s.advanceBy(len(prefix))
	}
	return
}

// consumeUntil consumes input until delimiter is prefix (returns true) or EOF
// (false). In either case, the delimiter itself has not been consumed.
func (s *scanner) consumeUntil(delimiter string) (found bool) {
	if x := strings.Index(s.input[s.pos:], delimiter); x >= 0 {
		found = true
		s.advanceBy(x)
	} else {
		// Jump to EOF
		s.pos = s.end
	}
	return
}
