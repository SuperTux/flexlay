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


from flexlay import TilemapLayer
from flexlay.math import Point
from flexlay.util import get_value_from_tree
from supertux.tileset import SuperTuxTileset


class SuperTuxTileMap:

    @staticmethod
    def from_sexpr(data):
        # Load position from path, node and then x, ys
        x = 0
        y = 0
        path = get_value_from_tree(["path"], data, None)
        if path:
            node = get_value_from_tree(["node"], path, None)
            if node:
                x = get_value_from_tree(["x", "_"], node, None)
                y = get_value_from_tree(["y", "_"], node, None)

        name = get_value_from_tree(["name", "_"], data, "")
        z_pos = get_value_from_tree(["z-pos", "_"], data, 0)
        solid = get_value_from_tree(["solid", "_"], data, False)

        result = SuperTuxTileMap(z_pos, solid)

        width = get_value_from_tree(["width", "_"], data, 20)
        height = get_value_from_tree(["height", "_"], data, 15)
        result.draw_target = get_value_from_tree(["draw-target", "_"], data, "")
        result.speed = get_value_from_tree(["speed", "_"], data, 1.0)
        result.speed_y = get_value_from_tree(["speed-y", "_"], data, 1.0)
        result.alpha = get_value_from_tree(["alpha", "_"], data, 1.0)

        result.tilemap_layer = TilemapLayer(SuperTuxTileset.current, width, height)
        result.tilemap_layer.set_data(get_value_from_tree(["tiles"], data, []))
        result.tilemap_layer.metadata = result
        result.pos = Point(x, y)
        result.tilemap_layer.name = name

        return result

    @staticmethod
    def from_size(width, height, name, z_pos=0, solid=False):
        result = SuperTuxTileMap(z_pos, solid)
        result.tilemap_layer = TilemapLayer(SuperTuxTileset.current, width, height)
        result.tilemap_layer.metadata = result
        result.tilemap_layer.name = name
        return result

    def __init__(self, z_pos=0, solid=False):
        self.solid = solid
        self.draw_target = ""
        self.z_pos = z_pos
        self.speed = 1.0
        self.speed_y = 1.0
        self.alpha = 1.0
        self.pos = Point(0, 0)
        self.tilemap_layer = None

    @property
    def name(self):
        return self.tilemap_layer.name

    def write(self, writer, objmap_tilemap_object):
        writer.begin_list("tilemap")
        writer.write_bool("solid", self.solid)
        if self.draw_target:
            writer.write_string("draw-target", self.draw_target)
        if self.speed != 1.0:
            writer.write_float("speed", self.speed)
        if self.speed_y != 1.0:
            writer.write_float("speed-y", self.speed_y)
        writer.write_int("z-pos", self.z_pos)
        if self.alpha != 1.0:
            writer.write_float("alpha", self.alpha)
        if self.tilemap_layer.name:
            writer.write_string("name", self.tilemap_layer.name)
        if self.pos and (self.pos.x != 0 or self.pos.y != 0):
            writer.begin_list("path")
            writer.begin_list("node")
            writer.write_int("x", self.pos.x)
            writer.write_int("y", self.pos.y)
            writer.end_list("node")
            writer.end_list("path")
        writer.write_int("width", self.tilemap_layer.width)
        writer.write_int("height", self.tilemap_layer.height)
        writer.write_field("tiles", self.tilemap_layer.field)
        writer.end_list()

    def get_bounding_rect(self):
        return self.tilemap_layer.get_bounding_rect()

    def has_bounding_rect(self):
        return self.tilemap_layer.has_bounding_rect()

    def draw(self, gc):
        self.tilemap_layer.draw(self.pos, gc)

    def world2tile(self, p):
        return self.tilemap_layer.world2tile(p)

    def get_tileset(self):
        return self.tilemap_layer.get_tileset()

    def get_data(self):
        return self.tilemap_layer.get_data()

    def set_data(self, data):
        self.tilemap_layer.set_data(data)

    def resize(self, size, pos):
        self.tilemap_layer.resize(size, pos)

    @property
    def field(self):
        return self.tilemap_layer.field

    @property
    def width(self):
        return self.tilemap_layer.width

    @property
    def height(self):
        return self.tilemap_layer.height


# EOF #
