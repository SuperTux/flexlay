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


from PyQt4.QtCore import QSize, Qt
from PyQt4.QtGui import QWidget
from PyQt4.QtGui import QPainter

from flexlay.math import Point, Rect, Size
from ..graphic_context import GraphicContext
from ..color import Color
from ..tileset import Tileset
from ..tile_brush import TileBrush
from ..tool_context import ToolContext


class TileSelectorWidget(QWidget):

    def __init__(self, viewport):
        super().__init__()

        self.viewport = viewport
        self.index = 0

        self.has_focus = False
        self.mouse_over_tile = -1
        self.region_select = False
        self.current_pos = Point()
        self.region_select_start = Point()
        self.mouse_pos = Point()
        self.scale = 1.0
        self.tileset = Tileset(32)
        self.tiles = []

        self.setMouseTracking(True)

    def get_selection(self):
        selection = Rect(self.current_pos.x, self.current_pos.y,
                         self.region_select_start.x, self.region_select_start.y)

        selection.normalize()
        selection.right += 1
        selection.bottom += 1

        selection.left = min(max(0, selection.left), self.columns)
        selection.right = min(max(0, selection.right), self.columns)

        selection.top = max(0, selection.top)

        return selection

    def mousePressEvent(self, event):
        if event.button() == Qt.LeftButton:
            brush = TileBrush(1, 1)

            brush.set_opaque()
            if 0 <= self.mouse_over_tile and self.mouse_over_tile < len(self.tiles):
                brush.put(0, 0, self.tiles[self.mouse_over_tile])
            else:
                brush.put(0, 0, 0)

            ToolContext.current.tile_brush = brush

        elif event.button() == Qt.RightButton:
            self.region_select = True
            self.region_select_start = self.current_pos
            self.grabMouse()

        self.repaint()

    def mouseReleaseEvent(self, event):

        if event.button() == Qt.RightButton:
            self.releaseMouse()
            self.region_select = False

            selection = self.get_selection()
            # selection.bottom = min(max(0, selection.right), self.columns)

            brush = TileBrush(selection.width, selection.height)
            brush.set_transparent()

            for y in range(0, selection.height):
                for x in range(0, selection.width):
                    tile = (selection.top + y) * self.columns + (selection.left + x)

                    if 0 <= tile and tile < len(self.tiles):
                        brush.put(x, y, self.tiles[tile])
                    else:
                        brush.put(x, y, 0)

            ToolContext.current.tile_brush = brush

        self.repaint()

    def mouseMoveEvent(self, event):
        pos = self.get_mouse_tile_pos(Point.from_qt(event.pos()))
        self.current_pos = pos
        self.mouse_over_tile = pos.y * self.columns + pos.x
        self.repaint()

    def get_mouse_tile_pos(self, mouse_pos):
        x = int(mouse_pos.x / self.cell_size)
        y = int(mouse_pos.y / self.cell_size)

        if x >= self.columns:
            x = self.columns - 1

        return Point(x, y)

    def paintEvent(self, event):
        painter = QPainter(self)
        gc = GraphicContext(painter)

        brush = ToolContext.current.tile_brush

        start_row = event.rect().top() // self.cell_size
        end_row = (event.rect().bottom() + self.cell_size - 1) // self.cell_size
        end_index = min(end_row * self.columns, len(self.tiles))

        # Draw tiles
        for i in range(start_row * self.columns, end_index):
            x = i % self.columns
            y = i // self.columns

            tile = self.tileset.create(self.tiles[i])

            rect = Rect(Point(int(x * self.cell_size),
                              int(y * self.cell_size)),
                        Size(self.cell_size,
                             self.cell_size))

            if tile:
                sprite = tile.get_sprite()
                sprite.set_scale(self.scale, self.scale)
                sprite.draw(int(x * self.cell_size),
                            int(y * self.cell_size), gc)

                # Use grid in the tileselector
                gc.draw_rect(rect, Color(0, 0, 0, 128))

            # mark the currently selected tile
            if brush.width == 1 and brush.height == 1 and \
               brush.at(0, 0) == self.tiles[i]:
                gc.fill_rect(rect, Color(0, 0, 255, 100))
            elif self.mouse_over_tile == i and self.has_focus:
                gc.fill_rect(rect, Color(0, 0, 255, 20))

        # draw rectangle selection
        if self.region_select:
            rect = self.get_selection()

            rect.top *= self.cell_size
            rect.bottom *= self.cell_size
            rect.left *= self.cell_size
            rect.right *= self.cell_size

            gc.fill_rect(rect, Color(0, 0, 255, 100))

    def enterEvent(self, event):
        self.has_focus = True
        self.repaint()

    def leaveEvent(self, event):
        self.has_focus = False
        self.repaint()

    def resizeEvent(self, event):
        self.update_minimum_size()

    def update_minimum_size(self):
        min_rows = (len(self.tiles) + self.columns - 1) / self.columns
        size = QSize(self.tileset.get_tile_size() * self.columns,
                     self.tileset.get_tile_size() * min_rows)
        self.setMinimumSize(size)

    @property
    def columns(self):
        return int(self.viewport.width() / self.cell_size)

    def set_scale(self, s):
        self.scale = s
        self.repaint()

    def get_tiles(self):
        return self.tiles

    def set_tileset(self, tileset):
        self.tileset = tileset
        self.update_minimum_size()

    def set_tiles(self, tiles):
        self.tiles = tiles
        self.update_minimum_size()

    @property
    def cell_size(self):
        return int(self.tileset.get_tile_size() * self.scale)


# EOF #
