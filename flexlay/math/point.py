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


from typing import Optional, Union, overload

from PyQt5.QtCore import QPoint


class Point:

    @staticmethod
    def from_qt(qpoint: QPoint) -> 'Point':
        return Point(qpoint.x(), qpoint.y())

    @overload
    def __init__(self) -> None:
        ...

    @overload
    def __init__(self, arg1: int, arg2: int) -> None:
        ...

    @overload
    def __init__(self, arg1: Point) -> None:
        ...

    def __init__(self, arg1: Optional[Union['Point', int]] = None, arg2: Optional[int] = None) -> None:
        if arg1 is None and arg2 is None:
            self.x: int = 0
            self.y: int = 0
        elif isinstance(arg1, Point) and arg2 is None:
            self.x = arg1.x
            self.y = arg1.y
        elif isinstance(arg1, int) and isinstance(arg2, int):
            self.x = arg1
            self.y = arg2
        else:
            raise ValueError("invalid types for Point.__init__()")

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

    def to_qt(self) -> QPoint:
        return QPoint(self.x, self.y)

    def __str__(self) -> str:
        return "Point({}, {})".format(self.x, self.y)


Pointf = Point


# EOF #
