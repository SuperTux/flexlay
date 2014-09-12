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


class Tileset:

    def __init__(self, tile_size):
        self.tiles = {}
        self.tile_size = tile_size

    def add_tile(self, tile_id, tile):
        self.tiles[tile_id] = tile

    def create(self, tile_id):
        return self.tiles.get(tile_id)

    def get_tile_size(self):
        return self.tile_size

    def get_tiles(self):
        return self.tiles.keys()


# EOF #
