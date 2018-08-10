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


import itertools

from flexlay import Field
from flexlay.math import Rect


class TileBrush:

    @staticmethod
    def from_field(field, w, h, pos_x, pos_y):
        return TileBrush(w, h, field.copy_region(pos_x, pos_y, w, h))

    def __init__(self, w, h, field=None):
        self.opaque = False
        if field is None:
            self.field = Field.from_size(w, h)
        else:
            self.field = field

    @property
    def width(self):
        return self.field.width

    @property
    def height(self):
        return self.field.height

    def set_data(self, data):
        self.field.set_data(data)

    def get_data(self, data):
        return self.field.get_data(data)

    def at(self, x, y):
        return self.field.at(x, y)

    def put(self, x, y, value):
        assert isinstance(value, int)
        return self.field.put(x, y, value)

    def resize(self, w, h, pos_x=0, pos_y=0):
        self.field.resize(w, h, pos_x, pos_y)

    def set_opaque(self):
        self.opaque = True

    def set_transparent(self):
        self.opaque = False

    def is_opaque(self):
        return self.opaque

    def auto_crop(self):
        rect = Rect(0, 0, 0, 0)

        for y, x in itertools.product(range(0, self.field.height),
                                      range(0, self.field.width)):
            if self.field.at(x, y) != 0:
                rect.top = y
                break

        for y, x in itertools.product(reversed(range(0, self.field.height)),
                                      range(0, self.field.width)):
            if self.field.at(x, y) != 0:
                rect.bottom = y + 1
                break

        for x, y in itertools.product(range(0, self.field.width),
                                      range(0, self.field.height)):
            if self.field.at(x, y) != 0:
                rect.left = x
                break

        for x, y in itertools.product(reversed(range(0, self.field.width)),
                                      range(0, self.field.height)):
            if self.field.at(x, y) != 0:
                rect.right = x + 1
                break

        if rect.width != 0:
            self.resize(rect.width, rect.height,
                        -rect.left, -rect.top)
        else:
            self.field = Field.from_size(1, 1)
            self.field.put(0, 0, 0)
            self.set_opaque()


# EOF #
