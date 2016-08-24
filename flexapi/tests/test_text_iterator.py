# Flexlay - A Generic 2D Game Editor
#
# ISC License
# Copyright (C) 2016 Karkus476 <karkus476@yahoo.com>
#
# Permission to use, copy, modify, and/or distribute this software for
# any purpose with or without fee is hereby granted, provided that the
# above copyright notice and this permission notice appear in all
# copies.
#
# THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL
# WARRANTIES WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED
# WARRANTIES OF MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE
# AUTHOR BE LIABLE FOR ANY SPECIAL, DIRECT, INDIRECT, OR ON SEQUENTIAL
# DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR
# PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER
# TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR
# PERFORMANCE OF THIS SOFTWARE.

import unittest

from flexapi.backends.text_iterator import TextIterator

class TextIteratorTestCase(unittest.TestCase):
    def test_set_index(self):
        self.it = TextIterator("0123456789\n0123456789")
        self.it.set_index(3)
        self.assertEqual(self.it.index, 3)
        self.assertEqual(self.it.char, "3")
        self.assertEqual(self.it.char_no, 4)

        self.it.set_index(9)
        self.assertEqual(self.it.index, 9)
        self.assertEqual(self.it.char, "9")
        self.assertEqual(self.it.char_no, 10)

        self.it.set_index(0)
        self.assertEqual(self.it.index, 0)
        self.assertEqual(self.it.char, "0")
        self.assertEqual(self.it.line_no, 1)
        self.assertEqual(self.it.char_no, 1)
        
        self.it.set_index(11)
        self.assertEqual(self.it.index, 11)
        self.assertEqual(self.it.char, "0")
        self.assertEqual(self.it.line_no, 2)
        self.assertEqual(self.it.char_no, 1)
        
        self.it.set_index(6)
        self.assertEqual(self.it.index, 6)
        self.assertEqual(self.it.char, "6")
        self.assertEqual(self.it.line_no, 1)

        self.assertRaises(IndexError, self.it.set_index, -1)
        self.assertRaises(IndexError, self.it.set_index, 22)

        self.it.set_index(21)
        self.assertEqual(self.it.index, -1)
        self.assertEqual(self.it.char, "")
        self.assertTrue(self.it.done)

    def test_ignore_regex(self):
        self.it = TextIterator("0123456789\n0123456789")
        import re
        self.assertTrue(self.it.ignore_regex(re.compile(r"[0-3]+")))
        self.assertEqual(self.it.index, 4)
        self.assertEqual(self.it.char, "4")
        self.assertEqual(self.it.line_no, 1)

        self.assertFalse(self.it.ignore_regex(re.compile(r"[7-9]")))
        self.assertEqual(self.it.index, 4)
        self.assertEqual(self.it.char, "4")

        self.assertTrue(self.it.ignore_regex(re.compile(r"[0-9]+\n0")))
        self.assertEqual(self.it.index, 12)
        self.assertEqual(self.it.char, "1")
        self.assertEqual(self.it.line_no, 2)
        self.assertFalse(self.it.done)
        
        self.assertTrue(self.it.ignore_regex(re.compile("[1-9]+")))
        self.assertEqual(self.it.index, -1)
        self.assertEqual(self.it.char, "")
        self.assertEqual(self.it.line_no, -1)
        self.assertTrue(self.it.done)

    def test_accept_string(self):
        self.it = TextIterator("0123456789\n0123456789")
        self.assertTrue(self.it.accept_string("0123"))
        self.assertEqual(self.it.index, 4)
        self.assertEqual(self.it.char, "4")
        self.assertEqual(self.it.accepted, "0123")

        self.assertFalse(self.it.accept_string("abc"))
        self.assertEqual(self.it.index, 4)
        self.assertEqual(self.it.char, "4")
        self.assertEqual(self.it.accepted, "0123")

        self.assertFalse(self.it.accept_string("45678910"))
        self.assertEqual(self.it.index, 4)
        self.assertEqual(self.it.char, "4")
        self.assertEqual(self.it.accepted, "0123")

        self.assertTrue(self.it.accept_string("456789\n0"))
        self.assertEqual(self.it.index, 12)
        self.assertEqual(self.it.char, "1")
        self.assertEqual(self.it.accepted, "456789\n0")
        self.assertEqual(self.it.line_no, 2)
        
        self.assertTrue(self.it.accept_string("123456789"))
        self.assertEqual(self.it.index, -1)
        self.assertEqual(self.it.char, "")
        self.assertTrue(self.it.done)

    def test_accept_regex(self):
        self.it = TextIterator("0123456789")
        import re
        self.assertTrue(self.it.accept_regex(re.compile(r"[0-3]+")))
        self.assertEqual(self.it.index, 4)
        self.assertEqual(self.it.char, "4")
        self.assertEqual(self.it.accepted, "0123")

        self.assertFalse(self.it.accept_regex(re.compile(r"[7-9]")))
        self.assertEqual(self.it.index, 4)
        self.assertEqual(self.it.char, "4")
        self.assertEqual(self.it.accepted, "0123")

        self.assertTrue(self.it.accept_regex(re.compile(r"[0-9]+")))
        self.assertEqual(self.it.index, -1)
        self.assertEqual(self.it.char, "")
        self.assertTrue(self.it.done)
        self.assertEqual(self.it.accepted, "456789")
        
    if __name__ == "__main__":
        unittest.main()
    
