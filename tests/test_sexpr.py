# Flexlay - A Generic 2D Game Editor
# Copyright (C) 2014 Ingo Ruhnke <grumbel@gmail.com>
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.


from flexlay.util import SExprWriter
from flexlay.util.sexpr_reader import get_value_from_tree, assoc_ref
from flexlay.util.sexpr import parse as sexpr_parse, SExprParseError

import io
import unittest


class SExprWriterTestCase(unittest.TestCase):

    def setUp(self):
        pass

    def tearDown(self):
        pass

    def test_assoc_ref(self):
        lst = [["one", 1],
               ["two", 2],
               ["three", 3]]
        self.assertEqual(assoc_ref(lst, "one"), [1])
        self.assertEqual(assoc_ref(lst, "two"), [2])
        self.assertEqual(assoc_ref(lst, "three"), [3])

    def test_get_value_from_tree(self):
        sexpr = [["supertux-level",
                  ["bool", False],
                  ["sublist",
                   ["int", 20]],
                  ["int", 15]]]

        result = get_value_from_tree(["supertux-level", "bool", "_"], sexpr, None)
        self.assertEqual(result, False)

        result = get_value_from_tree(["supertux-level", "int", "_"], sexpr, None)
        self.assertEqual(result, 15)

        result = get_value_from_tree(["supertux-level", "sublist", "int", "_"], sexpr, None)
        self.assertEqual(result, 20)

    def test_sexpr_parser(self):
        result = sexpr_parse("")
        self.assertEqual(result, [])

        result = sexpr_parse("  ")
        self.assertEqual(result, [])

        result = sexpr_parse(" 123 ")
        self.assertEqual(result, [123])

        result = sexpr_parse("512323")
        self.assertEqual(result, [512323])

        result = sexpr_parse("(8(8)8)")
        self.assertEqual(result, [[8, [8], 8]])

        result = sexpr_parse("symbol")
        self.assertEqual(result, ["symbol"])

        result = sexpr_parse(r'(() ("bar" foo) ()) () bar ')
        self.assertEqual(result, [[[], ["bar", "foo"], []], [], "bar"])

        result = sexpr_parse((';;comment\n'
                              '("Hello World" 5 1 123) ("Hello" 123 123 "foobar") ;; comment'))
        self.assertEqual(result, [["Hello World", 5, 1, 123], ["Hello", 123, 123, "foobar"]])

        with self.assertRaises(SExprParseError):
            sexpr_parse("(")

        with self.assertRaises(SExprParseError):
            sexpr_parse("#  ")

        with self.assertRaises(SExprParseError):
            sexpr_parse("#b")

        with self.assertRaises(SExprParseError):
            sexpr_parse("\"unterminated string")

    def test_sexpr_writer(self):
        with io.StringIO() as out:
            writer = SExprWriter(out)
            writer.write_comment("This is a comment")
            writer.start_list("supertux-level")
            writer.start_list("empty-section")
            writer.end_list("empty-section")
            writer.start_list("test-section")
            writer.write_bool("falsebool", False)
            writer.write_bool("falsebool", True)
            writer.write_int("intvalue", 45)
            writer.write_float("floatvalue", 45.0)
            writer.write_string("astring", "a string")
            writer.write_vector("astring", [1, 2, 3, 5, 10, "a string"])
            writer.end_list("test-section")
            writer.end_list("supertux-level")
            result = out.getvalue()
            expected = (";; This is a comment\n"
                        "(supertux-level\n"
                        "  (empty-section\n"
                        "  )\n"
                        "  (test-section\n"
                        "    (falsebool #f)\n"
                        "    (falsebool #t)\n"
                        "    (intvalue 45)\n"
                        "    (floatvalue 45.0)\n"
                        "    (astring \"a string\")\n"
                        "    (astring 1 2 3 5 10 'a string')\n"
                        "  )\n"
                        ")\n")
            self.assertEqual(result, expected)


if __name__ == '__main__':
    unittest.main()


# EOF #
