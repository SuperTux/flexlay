#!/usr/bin/env python3

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


from flexlay.util import Signal

import unittest


class SignalTestCase(unittest.TestCase):

    def setUp(self):
        pass

    def tearDown(self):
        pass

    def test_signal_args(self):
        signal = Signal()
        x = 0
        def add_n(n):
            nonlocal x
            x += n
        signal.connect(lambda n: add_n(n))
        signal.connect(lambda n: add_n(n+2))
        signal(9)
        self.assertEqual(x, 20)

    def test_signal_connect(self):
        signal = Signal()
        x = 0
        def add_two():
            nonlocal x
            x += 2
        signal.connect(add_two)
        signal.connect(add_two)
        signal()
        self.assertEqual(x, 4)


    def test_signal_disconnect(self):
        signal = Signal()
        x = 0
        def add_two():
            nonlocal x
            x += 2
        def add_three():
            nonlocal x
            x += 3
        signal.connect(add_two)
        signal.connect(add_three)
        signal.disconnect(add_two)
        signal()
        self.assertEqual(x, 3)


if __name__ == '__main__':
    unittest.main()


# EOF #
