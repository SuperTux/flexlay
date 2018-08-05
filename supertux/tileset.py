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


import os
import logging

from flexlay import Tile, Tileset, PixelBuffer, Config
from flexlay.util import get_value_from_tree
from supertux.util import load_lisp


class TileGroup:
    def __init__(self, name, tiles):
        self.name = name
        self.tiles = tiles


# Load game tiles from filename into tileset
class SuperTuxTileset(Tileset):
    current = None
    filename = ""

    def __init__(self, *params):
        super().__init__(*params)
        SuperTuxTileset.current = self
        self.tilegroups = []
        self.filename = ""

    def create_ungrouped_tiles_group(self):
        self.tilegroups.append(TileGroup("Ungrouped Tiles", self.get_ungrouped_tiles()))

    def get_ungrouped_tiles(self):
        # Searches for tiles which are not yet grouped and creates a group
        # for them
        ungrouped_tiles = []
        for tile in self.get_tiles():
            ungrouped = True
            for group in self.tilegroups:
                if tile not in group.tiles:
                    ungrouped = False
                    break
            if ungrouped:
                ungrouped_tiles.append(tile)
        return ungrouped_tiles

    def load(self, filename):
        logging.info("Loading Tileset: %s" % filename)
        self.filename = filename
        tree = load_lisp(filename, "supertux-tiles")

        tree = tree[1:]

        for i in tree:
            if i[0] == "tiles":
                data = i[1:]
                width = get_value_from_tree(['width', '_'], data, 1)
                # height = get_value_from_tree(['height', '_'], data, 1)
                ids = get_value_from_tree(['ids'], data, [])
                # attributes = get_value_from_tree(['attributes'], data, [])
                image = get_value_from_tree(['image', '_'], data, None)

                if not image:
                    image = get_value_from_tree(['images', '_'], data, None)

                if not image:
                    image = get_value_from_tree(['editor-images', '_'], data, "tiles/auxiliary/notile.png")

                x = 0
                y = 0
                for tile_id in ids:
                    pixelbuffer = PixelBuffer.subregion_from_file(os.path.join(Config.current.datadir, "images", image),
                                                                  x * 32, y * 32, 32, 32)
                    self.add_tile(tile_id, Tile(pixelbuffer))
                    x += 1
                    if x == width:
                        x = 0
                        y += 1

            elif i[0] == "tile":
                data = i[1:]
                tile_id = get_value_from_tree(['id', '_'], data, -1)
                image = get_value_from_tree(['editor-images', '_'], data, False)
                hidden = get_value_from_tree(['hidden', '_'], data, False)

                if not image:
                    image = get_value_from_tree(['images', '_'], data, "tiles/auxiliary/notile.png")

                if isinstance(image, str):
                    pixelbuffer = PixelBuffer.from_file(
                        os.path.join(Config.current.datadir, "images", image))
                elif isinstance(image, list):
                    if image[0] == "region":
                        pixelbuffer = PixelBuffer.subregion_from_file(
                            os.path.join(Config.current.datadir, "images", image[1]),
                            image[2], image[3], image[4], image[5])

                if not hidden:
                    if tile_id == 0 or not pixelbuffer:
                        self.add_tile(tile_id, None)
                    else:
                        self.add_tile(tile_id, Tile(pixelbuffer))

            elif i[0] == "tilegroup":
                data = i[1:]
                name = get_value_from_tree(['name', '_'], data, "Unnamed")
                tiles = get_value_from_tree(['tiles'], data, [])

                if not self.tilegroups:
                    self.tilegroups = []
                self.tilegroups.append(TileGroup(name, tiles))


# EOF #
