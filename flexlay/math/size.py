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


from PyQt5.QCore import QSize, QSizeF


class Size:

    def __init__(self, w: int, h: int) -> None:
        self.width: int = w
        self.height: int = h

    def copy(self) -> 'Size':
        return Size(self.width, self.height)

    def to_qt(self) -> QSize:
        return QSize(self.width, self.height)

    def __eq__(self, rhs: object) -> bool:
        if not isinstance(rhs, Size):
            return False

        return (self.width == rhs.width and
                self.height == rhs.height)

    def __ne__(self, rhs: object) -> bool:
        return not self.__eq__(rhs)

    def __str__(self) -> str:
        return "Size({}, {})".format(self.width, self.height)


class Sizef:

    def __init__(self, w: float, h: float) -> None:
        self.width: float = w
        self.height: float = h

    def copy(self) -> 'Sizef':
        return Sizef(self.width, self.height)

    def to_qt(self) -> QSize:
        return QSizeF(self.width, self.height)

    def __eq__(self, rhs: object) -> bool:
        if not isinstance(rhs, Sizef):
            return False

        return (self.width == rhs.width and
                self.height == rhs.height)

    def __ne__(self, rhs: object) -> bool:
        return not self.__eq__(rhs)

    def __str__(self) -> str:
        return "Size({}, {})".format(self.width, self.height)


# EOF #
