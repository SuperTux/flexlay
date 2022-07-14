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


from typing import Any

from flexlay.blitter import blit
from flexlay.color import Color
from flexlay.field import Field
from flexlay.layer import Layer
from flexlay.math import Point, Pointf, Size, Sizef, Rectf
from flexlay.pixel_buffer import PixelBuffer
from flexlay.tile_brush import TileBrush
from flexlay.tileset import Tileset
from flexlay.graphic_context import GraphicContext


class TilemapLayer(Layer):

    def __init__(self, tileset: Tileset, w: int, h: int) -> None:
        super().__init__()

        self.name = "<no name>"

        self.tileset = tileset
        self.field = Field.from_size(w, h)

        self.background_color = Color(0, 0, 0, 0)
        self.foreground_color = Color(255, 255, 255, 255)

        # FIXME: Move this to the widget or to some more generic map-properties thingy
        self.draw_grid = False
        self.draw_attribute: bool = False

        self.metadata: Any = None

        # Do not touch! Use LayerSelector to hide/show layers.

        self.hidden = False

        for y in range(0, self.field.height):
            for x in range(0, self.field.width):
                self.field.put(x, y, 0)

    def draw(self, gc: GraphicContext, pos: Point = Point(0, 0)) -> None:
        if self.hidden:
            return
        tile_size = self.tileset.get_tile_size()

        if False and self.background_color.get_alpha() != 0:  # type: ignore[unreachable]
            gc.fill_rect(Rectf.from_ps(pos,  # type: ignore[unreachable]
                                       Sizef(self.field.width * tile_size,
                                             self.field.height * tile_size)),
                         self.background_color)

        # The visible rectangle
        rect = gc.get_clip_rect().to_i()

        # max() here stops tiles off screen (below 0) from being drawn
        start_x = max(0, pos.x // tile_size)
        start_y = max(0, pos.y // tile_size)

        # min() here stops tiles off screen (above size of clip rect) from being drawn
        end_x = min(self.field.width + pos.x // tile_size, rect.right // tile_size + 1)
        end_y = min(self.field.height + pos.y // tile_size, rect.bottom // tile_size + 1)

        if self.foreground_color != Color(255, 255, 255, 255):
            for y in range(start_y, end_y):
                for x in range(start_x, end_x):
                    # The coordinates on the field are from 0 - field.width
                    tile_id = self.field.at(x - start_x, y - start_y)
                    if tile_id:
                        tile = self.tileset.create(tile_id)
                        if tile:  # skip transparent tile for faster draw
                            sprite = tile.get_sprite()
                            sprite.set_color(self.foreground_color)
                            sprite.draw(x * tile_size, y * tile_size, gc)

                            if self.draw_attribute:
                                gc.fill_rect(Rectf.from_ps(Pointf(x, y),
                                                           Sizef(self.tileset.get_tile_size(),
                                                                 self.tileset.get_tile_size())),
                                             tile.get_attribute_color())
        else:
            for y in range(start_y, end_y):
                for x in range(start_x, end_x):
                    tile_id = self.field.at(x - start_x, y - start_y)
                    if tile_id:  # skip transparent tile for faster draw
                        tile = self.tileset.create(self.field.at(x - start_x, y - start_y))
                        if tile:
                            tile.get_sprite().draw(x * tile_size, y * tile_size, gc)

                            if self.draw_attribute:
                                gc.fill_rect(Rectf.from_ps(Pointf(x, y),
                                                           Sizef(self.tileset.get_tile_size(),
                                                                 self.tileset.get_tile_size())),
                                             tile.get_attribute_color())

        if self.draw_grid:
            for y in range(start_y, end_y):
                gc.draw_line(start_x * tile_size,
                             y * tile_size,
                             end_x * tile_size,
                             y * tile_size,
                             Color(150, 150, 150))

            for x in range(start_x, end_x):
                gc.draw_line(x * tile_size,
                             start_y * tile_size,
                             x * tile_size,
                             end_y * tile_size,
                             Color(150, 150, 150))

    def get_tile(self, x: int, y: int) -> int:
        if 0 <= x < self.field.width and 0 <= y < self.field.height:
            return self.field.at(x, y)
        else:
            return 0

    def resize(self, size: Size, point: Point) -> None:
        self.field.resize(size.width, size.height, point.x, point.y)

    def replace_tile(self, source_id: int, replacement_id: int) -> None:
        for y in range(self.field.height):
            for x in range(self.field.width):
                if self.field.at(x, y) == source_id:
                    self.field.put(x, y, replacement_id)

    def flood_fill_at(self, pos: Point, brush: TileBrush) -> None:
        replace_id = self.field.at(pos.x, pos.y)
        if replace_id not in brush.field:
            self._flood_fill_at(pos.x, pos.y, brush, replace_id)

    def _flood_fill_at(self, orig_x: int, orig_y: int, brush: TileBrush, replace_id: int) -> None:
        stack = [(orig_x, orig_y)]

        def add(x: int, y: int) -> None:
            if 0 <= x < self.field.width and 0 <= y < self.field.height:
                stack.append((x, y))

        while stack:
            x, y = stack.pop()
            tile_id = self.field.at(x, y)
            if tile_id == replace_id:
                fill_id = brush.at((x - orig_x) % brush.width,
                                   (y - orig_y) % brush.height)
                self.field.put(x, y, fill_id)
                add(x + 1, y)
                add(x - 1, y)
                add(x, y + 1)
                add(x, y - 1)

    def draw_tile(self, tile_id: int, pos: Point) -> None:
        assert isinstance(tile_id, int)
        if 0 <= pos.x < self.field.width and 0 <= pos.y < self.field.height:
            self.field.put(pos.x, pos.y, tile_id)

    # formerly draw_tile()
    def draw_tile_brush(self, brush: TileBrush, pos: Point) -> None:
        self.draw_tiles(self.field, brush, pos)

    @staticmethod
    def draw_tiles(field: Field, brush: TileBrush, pos: Point) -> None:
        start_x = max(0, -pos.x)
        start_y = max(0, -pos.y)

        end_x = min(brush.width, field.width - pos.x)
        end_y = min(brush.height, field.height - pos.y)

        for y in range(start_y, end_y):
            for x in range(start_x, end_x):
                if brush.is_opaque() or brush.at(x, y) != 0:
                    field.put(pos.x + x, pos.y + y, brush.at(x, y))

    def set_draw_attribute(self, draw_attribute: int) -> None:
        self.draw_attribute = draw_attribute

    def get_draw_attribute(self) -> bool:
        return self.draw_attribute

    def set_draw_grid(self, t: bool) -> None:
        self.draw_grid = t

    def get_draw_grid(self) -> bool:
        return self.draw_grid

    def create_pixelbuffer(self) -> PixelBuffer:
        tile_size = self.tileset.get_tile_size()

        pixelbuffer = PixelBuffer.from_size(Size(self.width * tile_size,
                                                 self.height * tile_size))

        pixelbuffer.lock()
        buf = pixelbuffer.get_data()

        width = pixelbuffer.width
        height = pixelbuffer.height

        # Draw a nice gradient
        for y in range(height):
            for x in range(width):
                buf[4 * (y * width + x) + 0] = 255
                buf[4 * (y * width + x) + 1] = 255
                buf[4 * (y * width + x) + 2] = 255 * y // height
                buf[4 * (y * width + x) + 3] = 255 * y // height

        pixelbuffer.unlock()

        for y in range(self.height):
            for x in range(self.width):
                tile = self.tileset.create(self.field.at(x, y))

                if tile:
                    buf = tile.get_pixelbuffer()
                    if buf:
                        blit(pixelbuffer, buf, x * tile_size, y * tile_size)

        return pixelbuffer

    def get_bounding_rect(self) -> Rectf:
        return Rectf(0, 0,
                     self.field.width * self.tileset.get_tile_size(),
                     self.field.height * self.tileset.get_tile_size())

    def world2tile(self, pos: Pointf) -> Point:
        x = int(pos.x / self.tileset.get_tile_size())
        y = int(pos.y / self.tileset.get_tile_size())

        return Point(x - 1 if (pos.x < 0) else x,
                     y - 1 if (pos.y < 0) else y)

    def get_tileset(self) -> Tileset:
        return self.tileset

    def get_data(self) -> list[int]:
        return list(self.field._data.flatten())

    def set_data(self, data: list[int]) -> None:
        self.field = Field.from_list(self.field.width, self.field.height, data)

    def set_background_color(self, color: Color) -> None:
        self.background_color = color

    def set_foreground_color(self, color: Color) -> None:
        self.foreground_color = color

    @property
    def width(self) -> int:
        return self.field.width

    @property
    def height(self) -> int:
        return self.field.height

    def has_bounding_rect(self) -> bool:
        return True


# EOF #
