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


datadir = None
tileset = None


class TileMap:

    def __init__(self):
        self.tilemaplayer

    def new_from_size(self, width, height):
        self.width = width
        self.height = height
        self.tilemaplayer = TilemapLayer(tileset, self.width, self.height)

    def parse(self, data):
        self.width = get_value_from_tree(["width", "_"], data, 10)
        self.height = get_value_from_tree(["height", "_"], data, 10)
        self.layer = get_value_from_tree(["layer", "_"], data, "interactive")
        self.solid = get_value_from_tree(["solid", "_"], data, True)
        self.speed = get_value_from_tree(["speed", "_"], data, 1.0)
        self.tilemaplayer = TilemapLayer(tileset, self.width, self.height)
        self.tilemaplayer.set_data(get_value_from_tree(["tiles"], data, []))

    def save(self, writer):
        writer.start_list("tilemap")
        writer.write_int("width", self.width)
        writer.write_int("height", self.height)
        writer.write_string("layer", self.layer)
        writer.write_bool("solid", self.solid)
        writer.write_float("speed", self.speed)
        writer.write_int_vector("tiles", self.tilemaplayer.get_data())
        writer.end_list("tilemap")


# EOF #
