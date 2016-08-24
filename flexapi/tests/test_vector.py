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

from flexapi.math.vector import Vector


class VectorTestCase(unittest.TestCase):
    def test_add(self):
        a = Vector(3, 4)
        b = Vector(5, 6)
        c = a + b
        self.assertEqual(c.x, 8)
        self.assertEqual(c.y, 10)
        a += b
        self.assertEqual(a.x, 8)
        self.assertEqual(a.y, 10)

        self.assertRaises(TypeError, lambda v: v + 5, a)
        self.assertRaises(TypeError, lambda v: v + "5", a)

    def test_sub(self):
        a = Vector(3, 4)
        b = Vector(5, 6)
        c = a - b
        self.assertEqual(c.x, -2)
        self.assertEqual(c.y, -2)
        a -= b
        self.assertEqual(a.x, -2)
        self.assertEqual(a.y, -2)

        self.assertRaises(TypeError, lambda v: v - 5, a)
        self.assertRaises(TypeError, lambda v: v + "5", a)

    def test_mul(self):
        a = Vector(3, 4)
        b = Vector(5, 6)
        i = 5
        c = a * b
        self.assertEqual(c.x, 15)
        self.assertEqual(c.y, 24)
        a *= b
        self.assertEqual(a.x, 15)
        self.assertEqual(a.y, 24)
        d = b * i
        self.assertEqual(d.x, 25)
        self.assertEqual(d.y, 30)

        self.assertRaises(TypeError, lambda v: v * "5", a)

    def test_div(self):
        a = Vector(5, 25)
        i = 5
        self.assertEqual((a / i).x, 1)
        self.assertEqual((a / i).y, 5)

        self.assertRaises(TypeError, lambda v: v / "5", a)
        self.assertRaises(TypeError, lambda v: v / Vector(3, 4), a)
