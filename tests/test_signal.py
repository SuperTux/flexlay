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


import unittest
import unittest.mock

from flexlay.util.signal import Signal


class SignalTestCase(unittest.TestCase):

    def setUp(self) -> None:
        pass

    def tearDown(self) -> None:
        pass

    def test_signal_args(self) -> None:
        signal = Signal()
        mock1 = unittest.mock.Mock()
        mock2 = unittest.mock.Mock()
        signal.connect(mock1)
        signal.connect(mock2)
        signal(9, 11, 13)
        mock1.assert_called_with(9, 11, 13)
        mock2.assert_called_with(9, 11, 13)
        self.assertEqual(mock1.call_count, 1)
        self.assertEqual(mock2.call_count, 1)

    def test_signal_connect(self) -> None:
        signal = Signal()
        mock1 = unittest.mock.Mock()
        mock2 = unittest.mock.Mock()
        signal.connect(mock1)
        signal.connect(mock2)
        signal()
        mock1.assert_called_with()
        mock2.assert_called_with()
        self.assertEqual(mock1.call_count, 1)
        self.assertEqual(mock2.call_count, 1)

    def test_signal_disconnect(self) -> None:
        signal = Signal()
        mock1 = unittest.mock.Mock()
        mock2 = unittest.mock.Mock()
        signal.connect(mock1)
        signal.connect(mock2)
        signal.disconnect(mock1)
        signal()
        self.assertEqual(mock1.call_count, 0)
        self.assertEqual(mock2.call_count, 1)


if __name__ == '__main__':
    unittest.main()


# EOF #
