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


from typing import Union, TypeVar

from PyQt5.QtCore import QPoint, QPointF


T = TypeVar('T')


class Point:

    @staticmethod
    def from_qt(qpoint: QPoint) -> 'Point':
        return Point(qpoint.x(), qpoint.y())

    def to_qt(self) -> QPoint:
        return QPoint(self.x, self.y)

    def to_f(self) -> 'Pointf':
        return Pointf(self.x, self.y)

    def __init__(self, x: int, y: int) -> None:
        self.x: int = x
        self.y: int = y

    def copy(self) -> 'Point':
        return Point(self.x, self.y)

    def __add__(self, rhs: 'Point') -> 'Point':
        return Point(self.x + rhs.x, self.y + rhs.y)

    def __sub__(self, rhs: 'Point') -> 'Point':
        return Point(self.x - rhs.x, self.y - rhs.y)

    def __mul__(self, rhs: int) -> 'Point':
        return Point(self.x * rhs, self.y * rhs)

    def __rmul__(self, rhs: int) -> 'Point':
        return Point(self.x * rhs, self.y * rhs)

    def __eq__(self, rhs: object) -> bool:
        if not isinstance(rhs, Point):
            return False

        return self.x == rhs.x and self.y == rhs.y

    def __ne__(self, rhs: object) -> bool:
        return not self.__eq__(rhs)

    def __str__(self) -> str:
        return "Point({}, {})".format(self.x, self.y)


class Pointf:

    @staticmethod
    def from_qt(qpoint: Union[QPoint, QPointF]) -> 'Pointf':
        return Pointf(qpoint.x(), qpoint.y())

    def to_qt(self) -> QPointF:
        return QPointF(self.x, self.y)

    def to_i(self) -> 'Point':
        return Point(int(self.x), int(self.y))

    def __init__(self, x: float, y: float) -> None:
        self.x: float = x
        self.y: float = y

    def copy(self) -> 'Pointf':
        return Pointf(self.x, self.y)

    def __add__(self, rhs: 'Pointf') -> 'Pointf':
        return Pointf(self.x + rhs.x, self.y + rhs.y)

    def __sub__(self, rhs: 'Pointf') -> 'Pointf':
        return Pointf(self.x - rhs.x, self.y - rhs.y)

    def __mul__(self, rhs: float) -> 'Pointf':
        return Pointf(self.x * rhs, self.y * rhs)

    def __rmul__(self, rhs: float) -> 'Pointf':
        return Pointf(self.x * rhs, self.y * rhs)

    def __eq__(self, rhs: object) -> bool:
        if not isinstance(rhs, Pointf):
            return False

        return self.x == rhs.x and self.y == rhs.y

    def __ne__(self, rhs: object) -> bool:
        return not self.__eq__(rhs)

    def __str__(self) -> str:
        return "Pointf({}, {})".format(self.x, self.y)


# EOF #
