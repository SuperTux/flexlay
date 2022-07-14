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


from PyQt5.QtGui import QColor


class Color:

    def __init__(self, r: int = 255, g: int = 255, b: int = 255, a: int = 255) -> None:
        self.r = r
        self.g = g
        self.b = b
        self.a = a

    def copy(self) -> 'Color':
        return Color(self.r, self.g, self.b, self.a)

    def get_red(self) -> int:
        return self.r

    def get_green(self) -> int:
        return self.g

    def get_blue(self) -> int:
        return self.b

    def get_alpha(self) -> int:
        return self.a

    def __eq__(self, rhs: object) -> bool:
        if not isinstance(rhs, Color):
            return False

        return (self.r == rhs.r and
                self.g == rhs.g and
                self.b == rhs.b and
                self.a == rhs.a)

    def __ne__(self, rhs: object) -> bool:
        return not self.__eq__(rhs)

    def to_qt(self) -> QColor:
        return QColor(self.r, self.g, self.b, self.a)

    def to_list(self) -> list[int]:
        return [self.r, self.g, self.b, self.a]

    def to_hex(self) -> str:
        return "#%02x%02x%02x" % (self.r, self.g, self.b)


class Colorf:

    def __init__(self, r: float = 1.0, g: float = 1.0, b: float = 1.0, a: float = 1.0) -> None:
        self.r = r
        self.g = g
        self.b = b
        self.a = a

    def to_list(self) -> list[float]:
        return [self.r, self.g, self.b, self.a]

    def to_color(self) -> 'Color':
        return Color(int(255 * self.r),
                     int(255 * self.g),
                     int(255 * self.b),
                     int(255 * self.a))

    def __eq__(self, other: object) -> bool:
        if not isinstance(other, Colorf):
            return False

        return (self.r == other.r and
                self.b == other.b and
                self.g == other.g and
                self.a == other.a)

    def to_i(self) -> Color:
        return Color(int(255 * self.r),
                     int(255 * self.g),
                     int(255 * self.b),
                     int(255 * self.a))

    def to_qt(self) -> QColor:
        return QColor.fromRgbF(self.r, self.g, self.b, self.a)


# EOF #
