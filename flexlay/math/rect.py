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

from flexlay.math.point import Point, Pointf
from flexlay.math.size import Size, Sizef


class Rect:

    @staticmethod
    def from_ps(point: Point, size: Size) -> 'Rect':
        return Rect(point.x,
                    point.y,
                    point.x + size.width,
                    point.y + size.height)

    @staticmethod
    def zero() -> 'Rect':
        return Rect(0, 0, 0, 0)

    def __init__(self, left: int, top: int, right: int, bottom: int) -> None:
        self.left = left
        self.top = top
        self.right = right
        self.bottom = bottom

    def copy(self) -> 'Rect':
        return Rect(self.left, self.top,
                    self.right, self.bottom)

    def __eq__(self, rhs: object) -> bool:
        if not isinstance(rhs, Rect):
            return False

        return (self.left == rhs.left and
                self.top == rhs.top and
                self.right == rhs.right and
                self.bottom == rhs.bottom)

    def __ne__(self, rhs: object) -> bool:
        return not self.__eq__(rhs)

    @property
    def width(self) -> int:
        return self.right - self.left

    @property
    def height(self) -> int:
        return self.bottom - self.top

    @property
    def size(self) -> Size:
        return Size(self.right - self.left, self.bottom - self.top)

    def is_inside(self, point: Point) -> bool:
        return (self.left <= point.x and point.x <= self.right and
                self.top <= point.y and point.y <= self.bottom)

    def is_overlapped(self, rect: 'Rect') -> bool:
        return (rect.left < self.right and rect.right > self.left and
                rect.top < self.bottom and rect.bottom > self.top)

    def set_size(self, size: Size) -> None:
        self.right = self.left + size.width
        self.bottom = self.top + size.height

    def normalize(self) -> None:
        if self.left > self.right:
            self.left, self.right = self.right, self.left
        if self.top > self.bottom:
            self.top, self.bottom = self.bottom, self.top

    def to_qt(self) -> QRect:
        return QRect(int(self.left), int(self.top),
                     int(self.width), int(self.height))

    def to_f(self) -> 'Rectf':
        return Rectf(self.left, self.top,
                     self.right, self.bottom)

    def __str__(self) -> str:
        return "Rect({}, {}, {}, {})".format(self.left, self.top, self.right, self.bottom)


class Rectf:

    @staticmethod
    def from_ps(point: Pointf, size: Sizef) -> 'Rectf':
        return Rectf(point.x,
                     point.y,
                     point.x + size.width,
                     point.y + size.height)

    @staticmethod
    def zero() -> 'Rectf':
        return Rectf(0, 0, 0, 0)

    def __init__(self, left: float, top: float, right: float, bottom: float) -> None:
        self.left: float = left
        self.top: float = top
        self.right: float = right
        self.bottom: float = bottom

    def copy(self) -> 'Rectf':
        return Rectf(self.left, self.top,
                     self.right, self.bottom)

    def __eq__(self, rhs: object) -> bool:
        if not isinstance(rhs, Rect):
            return False

        return (self.left == rhs.left and
                self.top == rhs.top and
                self.right == rhs.right and
                self.bottom == rhs.bottom)

    def __ne__(self, rhs: object) -> bool:
        return not self.__eq__(rhs)

    @property
    def width(self) -> float:
        return self.right - self.left

    @property
    def height(self) -> float:
        return self.bottom - self.top

    @property
    def size(self) -> Sizef:
        return Sizef(self.right - self.left, self.bottom - self.top)

    def is_inside(self, point: Pointf) -> bool:
        return (self.left <= point.x and point.x <= self.right and
                self.top <= point.y and point.y <= self.bottom)

    def is_overlapped(self, rect: 'Rectf') -> bool:
        return (rect.left < self.right and rect.right > self.left and
                rect.top < self.bottom and rect.bottom > self.top)

    def set_size(self, size: Sizef) -> None:
        self.right = self.left + size.width
        self.bottom = self.top + size.height

    def normalize(self) -> None:
        if self.left > self.right:
            self.left, self.right = self.right, self.left
        if self.top > self.bottom:
            self.top, self.bottom = self.bottom, self.top

    def to_qt(self) -> QRectF:
        return QRectF(self.left, self.top,
                      self.width, self.height)

    def to_i(self) -> Rect:
        return Rect(int(self.left), int(self.top),
                    int(self.width), int(self.height))

    def __str__(self) -> str:
        return "Rectf({}, {}, {}, {})".format(self.left, self.top, self.right, self.bottom)


# EOF #
