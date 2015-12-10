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


from PyQt4.QtCore import QPoint


class Point:
    @staticmethod
    def from_qt(qpoint):
        return Point(qpoint.x(), qpoint.y())

    def __init__(self, arg1=None, arg2=None):
        if arg1 is None and arg2 is None:
            self.x = 0
            self.y = 0
        elif arg2 is None:
            self.x = arg1.x
            self.y = arg1.y
        else:
            self.x = arg1
            self.y = arg2

    def copy(self):
        return Point(self.x, self.y)

    def __add__(self, rhs):
        return Point(self.x + rhs.x, self.y + rhs.y)

    def __sub__(self, rhs):
        return Point(self.x - rhs.x, self.y - rhs.y)

    def __mul__(self, rhs):
        return Point(self.x * rhs, self.y * rhs)

    def __rmul__(self, rhs):
        return Point(self.x * rhs, self.y * rhs)

    def __eq__(self, rhs):
        return self.x == rhs.x and self.y == rhs.y

    def __ne__(self, rhs):
        return not self.__eq__(rhs)

    def to_qt(self):
        return QPoint(self.x, self.y)

    def __str__(self):
        return "Point({}, {})".format(self.x, self.y)


Pointf = Point


# EOF #
