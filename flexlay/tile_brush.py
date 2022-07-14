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


from typing import Optional

import itertools

from flexlay.field import Field
from flexlay.math import Rect


class TileBrush:

    @staticmethod
    def from_field(field: Field, w: int, h: int, pos_x: int, pos_y: int) -> 'TileBrush':
        return TileBrush(w, h, field.copy_region(pos_x, pos_y, w, h))

    def __init__(self, w: int, h: int, field: Optional[Field] = None) -> None:
        self.opaque = False
        if field is None:
            self.field = Field.from_size(w, h)
        else:
            self.field = field

    @property
    def width(self) -> int:
        return self.field.width

    @property
    def height(self) -> int:
        return self.field.height

    def at(self, x: int, y: int) -> int:
        return self.field.at(x, y)

    def put(self, x: int, y: int, value: int) -> None:
        assert isinstance(value, int)
        self.field.put(x, y, value)

    def resize(self, w: int, h: int, pos_x: int = 0, pos_y: int = 0) -> None:
        self.field.resize(w, h, pos_x, pos_y)

    def set_opaque(self) -> None:
        self.opaque = True

    def set_transparent(self) -> None:
        self.opaque = False

    def is_opaque(self) -> bool:
        return self.opaque

    def auto_crop(self) -> None:
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
