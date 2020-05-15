// Copyright 2020 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package parse

import (
	"strings"
	"testing"
)

func it(msg string, cond bool, t *testing.T) {
	t.Helper()
	if !cond {
		t.Fatalf(msg)
	}
}

func TestScanset(t *testing.T) {
	punc := scanset(".,")
	it("should contain all its characters", punc.contains('.') && punc.contains(','), t)
	it("shold not contain anything else", !punc.contains('!'), t)
	it("should not match eof", !punc.contains(eof), t)
	it("should not match non-ASCII", !punc.contains(128), t)

	all := scanset("")
	it("should have a length of 128", len(all) == 128, t)
	for i := range all {
		all[i] = true
		it("should match any character when full", all.contains(rune(i)), t)
	}
	it("should not match eof even when full", !punc.contains(eof), t)
	it("should not match eof non-ASCII when full", !punc.contains(128), t)
}

func TestEmpty(t *testing.T) {
	s := scan("")
	it("should foresee EOF", s.peek() == eof, t)
	it("should have an empty itemString", s.itemString() == "", t)
	it("should start capturing at line 1", s.capture(itemSpace).line == 1, t)
}

func TestBasicNavigationAndMatching(t *testing.T) {
	input := "↑a↓cde"
	runes := []rune(input)

	s := scan(input)
	it("should peek() the first rune", s.peek() == runes[0], t)

	r, w := s.peek(), s.next()
	it("should next() the same rune", r == runes[0], t)
	it("and peek the second rune", s.peek() == runes[1], t)
	it("should have an itemString of the first rune", s.itemString() == string(runes[0]), t)

	s.backup(w)
	it("should go back to the first after backup()", s.peek() == runes[0], t)
	it("with an empty itemString", s.itemString() == "", t)
	it("and see a prefix of the whole string", s.hasPrefix(input), t)
	it("or of a lesser prefix", s.hasPrefix(string(runes[0:2])), t)

	_, _, _ = s.next(), s.peek(), s.next()
	it("should see the third after advancing two", s.peek() == runes[2], t)
	it("should fail to accept a different character", !s.accept(' '), t)
	it("should accept the correct character", s.accept(runes[2]), t)

	s.advanceBy(len(strings.TrimPrefix(input, s.itemString())))
	it("and be at EOF after advancing the remainder", s.peek() == eof, t)

	r, w = s.peek(), s.next()
	it("and peek+next() should return eof/0", r == eof && w == 0, t)
	it("should have an itemString of the whole string", s.itemString() == input, t)
}

func TestItemsDelimitersAndLineTracking(t *testing.T) {
	s := scan("line 1\nline two\nline three")
	it("doesn't have an item to start with", !s.haveItem(), t)
	it("should find a line feed", s.acceptUntil("\n"), t)
	it("but not have consumed it", s.accept('\n'), t)
	it("should have an item", s.haveItem(), t)
	it("that's the first line", s.itemString() == "line 1\n", t)
	item := s.capture(itemText)
	it("should capture the first line", item.val == "line 1\n", t)
	it("with a start position of 0", item.pos == 0, t)
	it("and a line of 1", item.line == 1, t)
	it("and a type of itemText", item.typ == itemText, t)
	it("should no longer have an item", !s.haveItem(), t)

	it("should find the next line feed", s.acceptUntil("\n"), t)
	it("but not have consumed it", s.accept('\n'), t)
	item2 := s.capture(itemString)
	it("should capture the second line", item2.val == "line two\n", t)
	it("with a line of 2", item2.line == 2, t)
	it("shouldn't have a fourth line", !s.acceptUntil("\n"), t)
	it("but should be at EOF", s.peek() == eof, t)
	item3 := s.capture(itemString)
	it("and capture the third line", item3.val == "line three", t)
	it("as line 3", item3.line == 3, t)
}

func TestAcceptEitherAndAny(t *testing.T) {
	s := scan("↑↑↓↓←→←→BA")
	s.peek()
	it("should accept only up or down (x4)",
		true &&
			s.peek() == '↑' && !s.acceptEither('←', '→') && s.acceptEither('↑', '↓') &&
			s.peek() == '↑' && !s.acceptEither('←', '→') && s.acceptEither('↑', '↓') &&
			s.peek() == '↓' && !s.acceptEither('←', '→') && s.acceptEither('↑', '↓') &&
			s.peek() == '↓' && !s.acceptEither('←', '→') && s.acceptEither('↑', '↓'),
		t,
	)
	it("should accept only left or right (x4)",
		true &&
			s.peek() == '←' && !s.acceptEither('↑', '↓') && s.acceptEither('←', '→') &&
			s.peek() == '→' && !s.acceptEither('↑', '↓') && s.acceptEither('←', '→') &&
			s.peek() == '←' && !s.acceptEither('↑', '↓') && s.acceptEither('←', '→') &&
			s.peek() == '→' && !s.acceptEither('↑', '↓') && s.acceptEither('←', '→'),
		t,
	)
	it("should accept any A or B (x2)",
		true &&
			!s.acceptAny(scanset("CD")) && s.acceptAny(scanset("AB")) &&
			!s.acceptAny(scanset("CD")) && s.acceptAny(scanset("AB")),
		t,
	)
}

func TestUnlimitedBackup(t *testing.T) {
	s := scan("A↑B↓C←D→")
	r1, w1 := s.peek(), s.next()
	r2, w2 := s.peek(), s.next()
	r3, w3 := s.peek(), s.next()
	it("should get A/1", r1 == 'A' && w1 == 1, t)
	it("should get ↑/2", r2 == '↑' && w2 == 3, t)
	it("should get B/1", r3 == 'B' && w3 == 1, t)
	s.backup(w3)
	it("should back up to B", s.peek() == 'B', t)
	s.backup(w2)
	it("should back up again to ↑", s.peek() == '↑', t)
	s.backup(w1)
	it("all the way to A", s.peek() == 'A', t)
}
