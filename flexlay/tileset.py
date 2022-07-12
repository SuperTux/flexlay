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


from typing import Optional

from flexlay.tile import Tile


class Tileset:

    def __init__(self, tile_size: int) -> None:
        self.tiles: dict[int, Optional[Tile]] = {}
        self.tile_size = tile_size

    def add_tile(self, tile_id: int, tile: Optional[Tile]) -> None:
        self.tiles[tile_id] = tile

    def create(self, tile_id: int) -> Tile:
        return self.tiles.get(tile_id)

    def get_tile_size(self) -> int:
        return self.tile_size

    def get_tiles(self) -> list[int]:
        return list(self.tiles.keys())


# EOF #
