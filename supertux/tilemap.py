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
from flexlay.util import get_value_from_tree


from .tileset import SuperTuxTileset


class SuperTuxTileMap:

    @staticmethod
    def from_sexpr(data):
        result = SuperTuxTileMap()
        width = get_value_from_tree(["width", "_"],  data, 20)
        height = get_value_from_tree(["height", "_"], data, 15)
        result.solid = get_value_from_tree(["solid", "_"],  data, False)
        result.z_pos = get_value_from_tree(["z-pos", "_"],  data, 0)
        result.speed = get_value_from_tree(["speed", "_"],  data, 1.0)
        result.speed_y = get_value_from_tree(["speed-y", "_"],  data, 1.0)
        result.alpha = get_value_from_tree(["alpha", "_"],  data, 255)
        result.name = get_value_from_tree(["name", "_"],  data, "")

        result.tilemap_layer = TilemapLayer(SuperTuxTileset.current, width, height)
        result.tilemap_layer.set_data(get_value_from_tree(["tiles"], data, []))
        result.tilemap_layer.metadata = result

        return result

    @staticmethod
    def from_size(width, height):
        result = SuperTuxTileMap()
        result.tilemap_layer = TilemapLayer(SuperTuxTileset.current, width, height)
        result.tilemap_layer.metadata = result
        return result

    def __init__(self):
        self.solid = True
        self.z_pos = 0
        self.name = ""
        self.speed = 1.0
        self.speed_y = 1.0
        self.name = "interactive"
        self.alpha = 255
        self.tilemap_layer = None

    def save(self, writer):
        writer.begin_list("tilemap")
        writer.write_bool("solid", self.solid)
        if self.alpha != 255:
            writer.write_int("alpha", self.alpha)
        if self.speed != 1.0:
            writer.write_float("speed", self.speed)
        if self.speed_y != 1.0:
            writer.write_float("speed-y", self.speed)
        writer.write_int("z-pos", self.z_pos)
        if self.name:
            writer.write_string("name", self.name)
        writer.write_int("width", self.tilemap_layer.width)
        writer.write_int("height", self.tilemap_layer.height)
        writer.write_field("tiles", self.tilemap_layer.field)
        writer.end_list()

    def get_bounding_rect(self):
        return self.tilemap_layer.get_bounding_rect()

    def has_bounding_rect(self):
        return self.tilemap_layer.has_bounding_rect()

    def draw(self, gc):
        self.tilemap_layer.draw(gc)

    def world2tile(self, p):
        return self.tilemap_layer.world2tile(p)

    def get_tileset(self):
        return self.tilemap_layer.get_tileset()

    def get_data(self):
        return self.tilemap_layer.get_data()

    def set_data(self, data):
        self.tilemap_layer.set_data(data)

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
