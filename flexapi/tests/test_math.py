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


import unittest

from flexlay.math import Point


class MathTestCase(unittest.TestCase):
    def test_point(self):
        p = Point(11, 17)
        self.assertEqual(p.x, 11)
        self.assertEqual(p.y, 17)

        p = 3 * p
        p = p * 4
        p *= 5
        self.assertEqual(p.x, 660)
        self.assertEqual(p.y, 1020)

        p = Point(0, 3) + p
        p = p + Point(4, 0)
        p += Point(1, 2)
        self.assertEqual(p.x, 665)
        self.assertEqual(p.y, 1025)

        self.assertEqual(Point(11, 17), Point(11, 17))

        p2 = p.copy()
        self.assertEqual(p, p2)
        p2.x += 2
        p2.y += 3
        self.assertNotEqual(p, p2)
