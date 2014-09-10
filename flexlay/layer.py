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


from flexlay.math import Pointf


class Layer:

    def __init__(self):
        self.data = None
        self.pos = Pointf(0, 0)

    def draw(self, gc):
        if self.pos.x != 0 or self.pos.y != 0:
            gc.push_modelview()
            gc.translate(self.pos.x, self.pos.y)
            self.draw(gc)
            gc.pop_modelview()
        else:
            self.draw(gc)

    def has_bounding_rect(self):
        return self.has_bounding_rect()

    def get_bounding_rect(self):
        rect = self.get_bounding_rect()
        rect.left += self.pos.x
        rect.top += self.pos.y
        rect.right += self.pos.x
        rect.bottom += self.pos.y
        return rect

    def get_metadata(self):
        return self.data

    def set_metadata(self, data):
        self.data = data

    def set_pos(self, pos):
        self.pos = pos

    def get_pos(self):
        return self.pos


# EOF #
