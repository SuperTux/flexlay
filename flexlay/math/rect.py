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


from PyQt5.Core import QRect
from flexlay.math import Size


class Rect:

    def __init__(self, left, top, right, bottom):
        self.left = left
        self.top = top
        self.right = right
        self.bottom = bottom

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

    def get_width(self):
        return self.right - self.left

    def get_height(self):
        return self.bottom - self.top

    def get_size(self):
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
                     self.get_width(), self.get_height())


# EOF #
