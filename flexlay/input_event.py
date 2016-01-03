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

import logging

from PyQt4.QtCore import Qt

from flexlay.math import Point


class InputEvent:
    MOUSE_LEFT = 1
    MOUSE_RIGHT = 2
    MOUSE_MIDDLE = 3
    MOUSE_WHEEL_UP = 4
    MOUSE_WHEEL_DOWN = 5
    MOUSE_NO_BUTTON = 6

    MOD_SHIFT = (1 << 0)
    MOD_CTRL = (1 << 1)
    MOD_ALT = (1 << 2)

    def __init__(self):
        self.kind = InputEvent.MOUSE_NO_BUTTON
        self.mouse_pos = None
        self.mod = 0

    @staticmethod
    def from_qt(event):
        result = InputEvent()

        result.mouse_pos = Point(event.x(), event.y())

        if event.button() == 0:
            result.kind = InputEvent.MOUSE_NO_BUTTON
        elif event.button() == Qt.LeftButton:
            result.kind = InputEvent.MOUSE_LEFT
        elif event.button() == Qt.MidButton:
            result.kind = InputEvent.MOUSE_MIDDLE
        elif event.button() == Qt.RightButton:
            result.kind = InputEvent.MOUSE_RIGHT
        else:
            logging.debug("unknown mouse button: " + str(event.button()))

        if event.modifiers() & Qt.ControlModifier:
            result.mod |= InputEvent.MOD_CTRL

        if event.modifiers() & Qt.AltModifier:
            result.mod |= InputEvent.MOD_ALT

        if event.modifiers() & Qt.ShiftModifier:
            result.mod |= InputEvent.MOD_SHIFT

        return result

# EOF #
