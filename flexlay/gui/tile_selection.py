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


from flexlay.math import Rect, TileBrush


class TileSelection:

    def __init__(self):
        self.tilemap = None
        self.start_pos = None
        self.selection = None
        self.active = False

    def start(self, tilemap, pos):
        self.tilemap = tilemap
        self.active = True
        self.start_pos = pos
        self.update(self.start_pos)

    def update(self, pos):
        self.selection = Rect(min(self.start_pos.x, pos.x),
                              min(self.start_pos.y, pos.y),
                              max(self.start_pos.x, pos.x) + 1,
                              max(self.start_pos.y, pos.y) + 1)

    def is_active(self):
        return self.active

    def clear(self):
        self.selection = Rect()
        self.active = False

    def draw(self, gc, color):
        tile_size = self.tilemap.get_tileset().get_tile_size()

        gc.fill_rect(Rect(self.selection.left * tile_size,
                          self.selection.top * tile_size,
                          self.selection.right * tile_size,
                          self.selection.bottom * tile_size),
                     color)

    def get_brush(self, field):
        sel = self.selection.copy()

        sel.normalize()

        if (sel.left > field.get_width() - 1 or
            sel.top > field.get_height() - 1 or
            sel.right <= 0 or
                sel.bottom <= 0):

            # Selection is empty
            print("Error: Invalid selection")
            brush = TileBrush(1, 1)
            brush.put(0, 0, 0)
            brush.set_opaque()
            return brush
        else:
            # Selection is valid
            # Cut the selection to the field size
            sel.left = max(0, sel.left)
            sel.top = max(0, sel.top)

            sel.right = min(sel.right,  field.get_width())
            sel.bottom = min(sel.bottom, field.get_height())

            brush = TileBrush(sel.get_width(), sel.get_height())

            for y in range(sel.top, sel.bottom):
                for x in range(sel.left, sel.right):
                    brush.put(x - sel.left, y - sel.top,
                              field.at(x, y))

            return brush

    def get_rect(self):
        sel = self.selection.copy()
        sel.normalize()
        return sel


# EOF #
