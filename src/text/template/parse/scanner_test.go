// Copyright 2020 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package parse

import (
	"testing"
)

func it(msg string, cond bool, t *testing.T) {
	t.Helper()
	if !cond {
		t.Fatalf(msg)
	}
}

func TestEmpty(t *testing.T) {
	s := scan("")
	it("should foresee EOF", s.peek() == eof, t)
	it("should have an empty itemString", s.itemString() == "", t)
	it("should start capturing at line 1", s.capture(itemSpace).line == 1, t)
}

func TestBasicNavigationAndMatching(t *testing.T) {
	s := scan("abc")
	it("should peek() the first rune", s.peek() == 'a', t)

	r, w := s.peek(), s.next()
	it("should next() the same rune", r == 'a', t)
	it("and peek the second rune", s.peek() == 'b', t)
	it("should have an itemString of the first rune", s.itemString() == "a", t)

	s.backup(w)
	it("should go back to the first after backup()", s.peek() == 'a', t)
	it("with an empty itemString", s.itemString() == "", t)
	it("and see a prefix of the whole string", s.hasPrefix("abc"), t)
	it("or of a lesser prefix", s.hasPrefix("ab"), t)

	s.advanceBy(2)
	it("should see the third after advancing two", s.peek() == 'c', t)
	it("should fail to accept a different character", !s.accept('b'), t)
	it("should accept the correct character", s.accept('c'), t)
	it("and be at EOF afterward", s.peek() == eof, t)

	r, w = s.peek(), s.next()
	it("and peek+next() should return eof/0", r == eof && w == 0, t)
	it("should have an itemString of the whole string", s.itemString() == "abc", t)
}

// items: capture, hasItem, startNewItem, line tracking in items
// accepts: acceptEither, acceptAny
// acceptUntil
// unlimited backup, utf8/navigational invariants
