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


class Size:

    def __init__(self, w, h):
        self.width = w
        self.height = h

    def copy(self):
        return Size(self.x, self.y)

    def __eq__(self, rhs):
        return (self.width == rhs.width and
                self.height == rhs.height)

    def __ne__(self, rhs):
        return not self.__eq__(rhs)

    def __str__(self):
        return "Size({}, {})".format(self.width, self.height)


Sizef = Size


# EOF #
