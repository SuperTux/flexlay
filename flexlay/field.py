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


class Field:

    def __init__(self, w, h, data=None):
        self.width = w
        self.height = h
        if data is None:
            self.data = [0] * w * h
        else:
            self.data = data

    def copy_region(self, x, y, w, h):
        start_x = max(0, -x)
        start_y = max(0, -y)

        end_x = min(self.width, w - x)
        end_y = min(self.height, h - y)

        field = Field(w, h)
        for iy in range(start_y, end_y):
            for ix in range(start_x, end_x):
                field.put(x + ix, y + iy, self.at(ix, iy))
        return field

    def copy(self):
        return Field(self.width, self.height, self.data[:])

    def put(self, x, y, value):
        assert isinstance(value, int)
        self.data[self.width * y + x] = value

    def at(self, x, y):
        assert (x >= 0 or x < self.width or y >= 0 or y < self.height)
        return self.data[self.width * y + x]

    def resize(self, w, h, x=0, y=0):
        field = self.copy_region(x, y, w, h)
        self.width = field.width
        self.height = field.height
        self.data = field.data

    def clear(self):
        self.width = 0
        self.height = 0
        self.data = []

    def get_data(self):
        return self.data

    def set_data(self, data):
        self.data = data[:]

    def size(self):
        return len(self.data)

    def __str__(self):
        result = "\n"
        for y in range(self.height):
            for x in range(self.width):
                result += "%04s " % self.at(x, y)
            result += "\n"
        return result


# EOF #
