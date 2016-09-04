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


class LevelTileset:
    def __init__(self, tile_size):
        self.tile_size = tile_size
        self.tiles = {0: None}
        self.groups = {"All Tiles": [0]}
        # Used by the TileSelectorWidget
        self.display_columns = 2
    
    def add_tile(self, tile_id, tile_image):
        """Maps an ImageResource to a tile id

        tiles cannot currently be removed
        """
        if not isinstance(tile_image, ImageResource):
            raise TypeError("tile_image must be ImageResource")
        if tile_id not in self.tiles:
            self.groups["All Tiles"].append(tile_id)
        self.tiles[tile_id] = tile_image
        
    def get_tile_image(self, id):
        """Get the ImageResource which is mapped to a certain id

        Returns None if no tile has such an id
        """
        try:
            return self.tiles[id]
        except IndexError:
            return None
