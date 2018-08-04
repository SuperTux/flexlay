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


from PyQt5.QtCore import QRect, QRectF

from .size import Size


class Rect:
    def __init__(self, arg1=None, arg2=None, arg3=None, arg4=None):
        if arg1 is None and arg2 is None and arg3 is None and arg4 is None:
            self.left = 0
            self.top = 0
            self.right = 0
            self.bottom = 0
        elif arg2 is None and arg3 is None and arg4 is None:
            self.left = int(arg1.left)
            self.top = int(arg1.top)
            self.right = int(arg1.right)
            self.bottom = int(arg1.bottom)
        elif arg3 is None and arg4 is None:
            self.left = arg1.x
            self.top = arg1.y
            self.right = arg1.x + arg2.width
            self.bottom = arg1.y + arg2.height
        else:
            self.left = arg1
            self.top = arg2
            self.right = arg3
            self.bottom = arg4

    def copy(self):
        return Rect(self.left, self.top,
                    self.right, self.bottom)

    def __eq__(self, rhs):
        return (self.left == rhs.left and
                self.top == rhs.top and
                self.right == rhs.right and
                self.bottom == rhs.bottom)

    def __ne__(self, rhs):
        return not self.__eq__(rhs)

    @property
    def width(self):
        return self.right - self.left

    @property
    def height(self):
        return self.bottom - self.top

    @property
    def size(self):
        return Size(self.right - self.left, self.bottom - self.top)

    def is_inside(self, point):
        return (self.left <= point.x and point.x <= self.right and
                self.top <= point.y and point.y <= self.bottom)

    def is_overlapped(self, rect):
        return (rect.left < self.right and rect.right > self.left and
                rect.top < self.bottom and rect.bottom > self.top)

    def set_size(self, size):
        self.right = self.left + size.width
        self.bottom = self.top + size.height

    def normalize(self):
        if self.left > self.right:
            self.left, self.right = self.right, self.left
        if self.top > self.bottom:
            self.top, self.bottom = self.bottom, self.top

    def to_qt(self):
        return QRect(self.left, self.top,
                     self.width, self.height)

    def to_qt_f(self):
        return QRectF(self.left, self.top,
                      self.width, self.height)

    def __str__(self):
        return "Rect({}, {}, {}, {})".format(self.left, self.top, self.right, self.bottom)


class Rectf(Rect):
    def __init__(self, arg1=None, arg2=None, arg3=None, arg4=None):
        super().__init__()

        if arg1 is None and arg2 is None and arg3 is None and arg4 is None:
            self.left = 0.0
            self.top = 0.0
            self.right = 0.0
            self.bottom = 0.0
        elif arg2 is None and arg3 is None and arg4 is None:
            self.left = arg1.left
            self.top = arg1.top
            self.right = arg1.right
            self.bottom = arg1.bottom
        elif arg3 is None and arg4 is None:
            self.left = arg1.x
            self.top = arg1.y
            self.right = arg1.x + arg2.width
            self.bottom = arg1.y + arg2.height
        else:
            self.left = arg1
            self.top = arg2
            self.right = arg3
            self.bottom = arg4

# EOF #
