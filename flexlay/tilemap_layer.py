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


from flexlay import Layer, Field, Color, PixelBuffer, blit
from flexlay.math import Point, Size, Rect


class TilemapLayer(Layer):

    def __init__(self, tileset, w, h):
        self.tileset = tileset
        self.field = Field < int > (w, h)

        self.background_color = Color(0, 0, 0, 0)
        self.foreground_color = Color(255, 255, 255, 255)

        # FIXME: Move this to the widget or to some more generic map-properties thingy
        self.draw_grid = False
        self.draw_attribute = False

        self.metadata = None

        for y in range(0, self.field.get_height()):
            for x in range(0, self.field.get_width()):
                self.field.put(x, y, 0)

    def draw(self, gc):
        tile_size = self.tileset.get_tile_size()

        if False and self.background_color.get_alpha() != 0:
            gc.fill_rect(Rect(Point(0, 0),
                              Size(self.field.get_width() * tile_size,
                                   self.field.get_height() * tile_size)),
                         self.background_color)

        rect = Rect(gc.get_clip_rect())

        start_x = max(0, rect.left / tile_size)
        start_y = max(0, rect.top / tile_size)
        end_x = min(self.field.get_width(),  rect.right / tile_size + 1)
        end_y = min(self.field.get_height(), rect.bottom / tile_size + 1)

        if self.foreground_color != Color(255, 255, 255, 255):
            for y in range(start_y, end_y):
                for x in range(start_x, end_x):
                    tile_id = self.field.at(x, y)
                    if tile_id:
                        tile = self.tileset.create(tile_id)
                        if tile:  # skip transparent tile for faster draw
                            sprite = tile.get_sprite()
                            sprite.set_color(self.foreground_color)
                            sprite.draw(x * tile_size, y * tile_size, gc)

                            if self.draw_attribute:
                                gc.fill_rect(Rect(Point(x, y), Size(self.tileset.get_tile_size(),
                                                                    self.tileset.get_tile_size())),
                                             tile.get_attribute_color())
        else:
            for y in range(start_y, end_y):
                for x in range(start_x, end_x):
                    tile_id = self.field.at(x, y)
                    if tile_id:  # skip transparent tile for faster draw
                        tile = self.tileset.create(self.field.at(x, y))
                        if tile:
                            tile.get_sprite().draw(x * tile_size, y * tile_size, gc)

                            if self.draw_attribute:
                                gc.fill_rect(Rect(Point(x, y), Size(self.tileset.get_tile_size(),
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

        gc.flush()

    def get_tile(self, x, y):
        if x >= 0 and x < self.field.get_width() and \
           y >= 0 and y < self.field.get_height():
            return self.field.at(x, y)
        else:
            return 0

    def resize(self, size, point):
        self.field.resize(size.width, size.height, point.x, point.y)

    def draw_tile(self, tile_id, pos):
        if pos.x >= 0 and pos.x < self.field.get_width() and \
           pos.y >= 0 and pos.y < self.field.get_height():
            self.field.put(pos.x, pos.y, tile_id)

    # formerly draw_tile()
    def draw_tile_brush(self, brush, pos):
        self.draw_tiles(self.field, brush, pos)

    def draw_tiles(self, field, brush, pos):
        start_x = max(0, -pos.x)
        start_y = max(0, -pos.y)

        end_x = min(brush.get_width(),  field.get_width() - pos.x)
        end_y = min(brush.get_height(), field.get_height() - pos.y)

        for y in range(start_y, end_y):
            for x in range(start_x, end_x):
                if (brush.is_opaque() or brush.at(x, y) != 0):
                    field.put(pos.x + x, pos.y + y, brush.at(x, y))

    def set_draw_attribute(self, t):
        self.draw_attribute = t

    def get_draw_attribute(self):
        return self.draw_attribute

    def set_draw_grid(self, t):
        self.draw_grid = t

    def get_draw_grid(self):
        return self.draw_grid

    def create_pixelbuffer(self):
        tile_size = self.tileset.get_tile_size()

        pixelbuffer = PixelBuffer(self.get_width() * tile_size,
                                  self.get_height() * tile_size)

        pixelbuffer.lock()
        buf = pixelbuffer.get_data()

        width = pixelbuffer.get_width()
        height = pixelbuffer.get_height()

        # Draw a nice gradient
        for y in range(height):
            for x in range(width):
                buf[4 * (y * width + x) + 0] = 255
                buf[4 * (y * width + x) + 1] = 255
                buf[4 * (y * width + x) + 2] = 255 * y / height
                buf[4 * (y * width + x) + 3] = 255 * y / height

        pixelbuffer.unlock()

        for y in range(self.get_height()):
            for x in range(self.get_width()):
                tile = self.tileset.create(self.field.at(x, y))

                if tile:
                    buf = tile.get_pixelbuffer()
                    if buf:
                        blit(pixelbuffer, buf, x * tile_size, y * tile_size)

        return pixelbuffer

    def get_bounding_rect(self):
        return Rect(Point(0, 0),
                    Size(self.field.get_width() * self.tileset.get_tile_size(),
                         self.field.get_height() * self.tileset.get_tile_size()))

    def world2tile(self, pos):
        x = int(pos.x / self.tileset.get_tile_size())
        y = int(pos.y / self.tileset.get_tile_size())

        return Point(x - 1 if (pos.x < 0) else x,
                     y - 1 if (pos.y < 0) else y)

    def get_field(self):
        return self.field

    def get_tileset(self):
        return self.tileset

    def get_data(self):
        return self.field.get_data()

    def set_data(self, data):
        self.field.set_data(data)

    def set_background_color(self, color):
        self.background_color = color

    def set_foreground_color(self, color):
        self.foreground_color = color

    def get_width(self):
        return self.field.get_width()

    def get_height(self):
        return self.field.get_height()

    def has_bounding_rect(self):
        return True

    def set_metadata(self, metadata):
        self.metadata = metadata

    def get_metadata(self):
        return self.metadata

# EOF #
