# Flexlay - A Generic 2D Game Editor
# Copyright (C) 2014 Ingo Ruhnke <grumbel@gmail.com>
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.


from typing import List, Optional


class Field:

    def __init__(self, w: int, h: int, data: Optional[List[int]]=None) -> None:
        self.width = w
        self.height = h
        if data is None:
            self.data = [0] * w * h
        else:
            self.data = data

    def copy_region(self, x: int, y: int, w: int, h: int) -> 'Field':
        start_x = max(0, -x)
        start_y = max(0, -y)

        end_x = min(self.width, w - x)
        end_y = min(self.height, h - y)

        field = Field(w, h)
        for iy in range(start_y, end_y):
            for ix in range(start_x, end_x):
                field.put(x + ix, y + iy, self.at(ix, iy))
        return field

    def copy(self) -> 'Field':
        return Field(self.width, self.height, self.data[:])

    def put(self, x: int, y: int, value: int) -> None:
        assert isinstance(value, int)
        self.data[self.width * y + x] = value

    def at(self, x: int, y: int) -> int:
        assert (x >= 0 or x < self.width or y >= 0 or y < self.height)
        return self.data[self.width * y + x]

    def resize(self, w: int, h: int, x: int=0, y: int=0) -> None:
        field = self.copy_region(x, y, w, h)
        self.width = field.width
        self.height = field.height
        self.data = field.data

    def clear(self) -> None:
        self.width = 0
        self.height = 0
        self.data = []

    def get_data(self) -> List[int]:
        return self.data

    def set_data(self, data: List[int]) -> None:
        self.data = data[:]

    def size(self) -> int:
        return len(self.data)

    def __contains__(self, key: int) -> bool:
        return key in self.data

    def __str__(self) -> str:
        result = "\n"
        for y in range(self.height):
            for x in range(self.width):
                result += "%04s " % self.at(x, y)
            result += "\n"
        return result


# EOF #
