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


from flexlay.math import Point, Size, Rect, Origin


class ObjMapControlPoint:

    def __init__(self, sprite, pos, metadata=None):
        self.sprite = sprite
        self.pos = pos
        self.metadata = metadata

        self.on_set_pos = None

    def sig_set_pos(self):
        return self.on_set_pos

    def draw(self, gc):
        self.sprite.draw(int(self.pos.x), int(self.pos.y), gc)

    def set_pos_raw(self, p):
        self.pos = p

    def set_pos(self, p):
        self.on_set_pos(p)

    def get_pos(self):
        return self.pos

    def get_bound_rect(self):
        origin_enum, align = self.sprite.get_alignment()
        align.x = -align.x
        origin = Origin.calc_origin(origin_enum,
                                    Size(self.sprite.get_width(),
                                         self.sprite.get_height()))

        return Rect(Point(self.pos) - origin - align,
                    Size(self.sprite.get_width(), self.sprite.get_height()))

# EOF #
