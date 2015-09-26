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


from flexlay.math import Pointf, Rectf, Sizef
from .objmap_object import ObjMapObject
# from .objmap_control_point import ObjMapControlPoint


class ObjMapTilemapObject(ObjMapObject):

    def __init__(self, tilemap_layer, metadata):
        super().__init__(Pointf(0, 0), metadata)

        self.tilemap_layer = tilemap_layer

    def draw(self, gc):
        self.tilemap_layer.draw(gc)

    def get_pos(self):
        return self.pos

    def is_inside(self, click_pos):
        return False

    def get_bound_rect(self):
        return Rectf(Pointf(0, 0),
                     Sizef(self.tilemap_layer.width * 32,
                           self.tilemap_layer.height * 32))

    def set_pos(self, pos):
        self.pos = pos

    def add_control_points(self):
        pass

    def update_control_points(self):
        pass


# EOF #
