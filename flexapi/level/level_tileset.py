# Flexlay - A Generic 2D Game Editor
#
# ISC License
# Copyright (C) 2016 Karkus476 <karkus476@yahoo.com>
#
# Permission to use, copy, modify, and/or distribute this software for
# any purpose with or without fee is hereby granted, provided that the
# above copyright notice and this permission notice appear in all
# copies.
#
# THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL
# WARRANTIES WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED
# WARRANTIES OF MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE
# AUTHOR BE LIABLE FOR ANY SPECIAL, DIRECT, INDIRECT, OR ON SEQUENTIAL
# DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR
# PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER
# TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR
# PERFORMANCE OF THIS SOFTWARE.

from flexapi.resources import ImageResource
from flexapi.flexlay_error import FlexlayError


class LevelTileset:
    def __init__(self, tile_size):
        self.tile_size = tile_size
        self.tiles = {0: None}
        self.groups = {"All Tiles": [0]}
        # Used by the TileSelectorWidget
        self.display_columns = 4
    
    def add_tile(self, tile_id, tile_image):
        """Maps an ImageResource to a tile id

        tiles cannot currently be removed
        """
        if not isinstance(tile_image, ImageResource):
            raise TypeError("tile_image must be ImageResource")
        if tile_id not in self.tiles:
            self.groups["All Tiles"].append(tile_id)
        self.tiles[tile_id] = tile_image

    def add_tilesheet(self, tile_ids, tiles_image):
        """Maps an ImageResource tilesheet to tile ids

        tile_ids is a list working from top left to bottom right,
        moving right each time and down to the start of the row.
        Each item should be an id.
        tiles cannot currently be removed
        """
        if not isinstance(tiles_image, ImageResource):
            raise TypeError("tile_image must be ImageResource")
        tiles_width = tiles_image.get_width() // self.tile_size
        tiles_height = tiles_image.get_height() // self.tile_size
        row_counter = 0
        column_counter = 0
        for tile_id in tile_ids:
            if row_counter >= tiles_height:
                raise FlexlayError("Too many tile_ids provided for tilesheet.")

            tile_image = tiles_image.get_subregion(column_counter * self.tile_size,
                                                   row_counter * self.tile_size,
                                                   self.tile_size,
                                                   self.tile_size)

            self.add_tile(tile_id, tile_image)

            if column_counter == tiles_width - 1:
                row_counter += 1
                column_counter = 0
            else:
                column_counter += 1
        
    def get_tile_image(self, id):
        """Get the ImageResource which is mapped to a certain id

        Returns None if no tile has such an id
        """
        try:
            return self.tiles[id]
        except IndexError:
            return None
