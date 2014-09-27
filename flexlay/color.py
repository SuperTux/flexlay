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


from PyQt4.QtGui import QColor


class Color:

    def __init__(self, r=255, g=255, b=255, a=255):
        self.r = r
        self.g = g
        self.b = b
        self.a = a

    def copy(self):
        return Color(self.r, self.g, self.b, self.a)

    def get_red(self):
        return self.r

    def get_green(self):
        return self.g

    def get_blue(self):
        return self.b

    def get_alpha(self):
        return self.a

    def __eq__(self, rhs):
        return (self.r == rhs.r and
                self.g == rhs.g and
                self.b == rhs.b and
                self.a == rhs.a)

    def __ne__(self, rhs):
        return not self.__eq__(rhs)

    def to_qt(self):
        return QColor(self.r, self.g, self.b, self.a)

    def to_list(self):
        return [self.r, self.g, self.b, self.a]


# EOF #
