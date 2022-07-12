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


from typing import Optional

import logging

from flexlay.color import Color
from flexlay.math import Point, Rect, Rectf
from flexlay.tile_brush import TileBrush
from flexlay.graphic_context import GraphicContext
from flexlay.field import Field
from flexlay.tilemap_layer import TilemapLayer


class TileSelection:

    def __init__(self) -> None:
        self.tilemap: Optional[TilemapLayer] = None
        self.start_pos: Optional[Point] = None
        self.selection: Optional[Rect] = None
        self.active: bool = False

    def start(self, tilemap: TilemapLayer, pos: Point) -> None:
        self.tilemap = tilemap
        self.active = True
        self.start_pos = pos
        self.update(self.start_pos)

    def update(self, pos: Point) -> None:
        if self.start_pos is None:
            return

        self.selection = Rect(min(self.start_pos.x, pos.x),
                              min(self.start_pos.y, pos.y),
                              max(self.start_pos.x, pos.x) + 1,
                              max(self.start_pos.y, pos.y) + 1)

    def is_active(self) -> bool:
        return self.active

    def clear(self) -> None:
        self.selection = Rect.zero()
        self.active = False

    def draw(self, gc: GraphicContext, color: Color = Color(255, 255, 255, 100)) -> None:
        if self.selection is None:
            return

        assert self.tilemap is not None
        tile_size = self.tilemap.get_tileset().get_tile_size()

        gc.fill_rect(Rectf(self.selection.left * tile_size,
                           self.selection.top * tile_size,
                           self.selection.right * tile_size,
                           self.selection.bottom * tile_size),
                     color)

    def get_brush(self, field: Field) -> TileBrush:
        assert self.selection is not None

        sel = self.selection.copy()

        sel.normalize()

        if sel.left > field.width - 1 or \
           sel.top > field.height - 1 or \
           sel.right <= 0 or \
           sel.bottom <= 0:

            # Selection is empty
            logging.error("Error: Invalid selection")
            brush = TileBrush(1, 1)
            brush.put(0, 0, 0)
            brush.set_opaque()
            return brush
        else:
            # Selection is valid
            # Cut the selection to the field size
            sel.left = max(0, sel.left)
            sel.top = max(0, sel.top)

            sel.right = min(sel.right, field.width)
            sel.bottom = min(sel.bottom, field.height)

            brush = TileBrush(sel.width, sel.height)

            for y in range(sel.top, sel.bottom):
                for x in range(sel.left, sel.right):
                    brush.put(x - sel.left, y - sel.top,
                              field.at(x, y))

            return brush

    def get_rect(self) -> Rect:
        assert self.selection is not None
        sel = self.selection.copy()
        sel.normalize()
        return sel


# EOF #
