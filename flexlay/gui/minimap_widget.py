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


from PyQt5.QtCore import QSize
from PyQt5.QtGui import QPainter
from PyQt5.QtWidgets import QWidget

from flexlay import Color, GraphicContext, TilemapLayer
from flexlay.math import Point, Size, Rect


class MinimapWidget(QWidget):

    def __init__(self, editormap_component):
        super().__init__()

        self.drag_active = False
        self.last_serial = -1
        self.editor_map = False
        self.parent = editormap_component
        self.minimap_surface = None

    def minimumSizeHint(self):
        return QSize(0, 0)

    def sizeHint(self):
        return QSize(300, 150)

    def paintEvent(self, event):
        if not self.parent.get_workspace().get_map():
            return

        if not self.parent or not self.parent.get_workspace():
            return

        painter = QPainter(self)
        gc = GraphicContext(painter)

        gc.push_modelview()

        # FIXME: Do this only on map changes
        if self.last_serial != self.parent.get_workspace().get_map().get_serial():
        # or editor_map != parent.get_workspace().get_map())
            self.update_minimap()
            self.last_serial = self.parent.get_workspace().get_map().get_serial()
            self.editor_map = self.parent.get_workspace().get_map()

        if True:
            # Draw background color
            gc.fill_rect(Rect(Point(0, 0), Size(self.width(), self.height())),
                         Color(200, 200, 200, 225))

        # FIXME: This doesn't work all that well
        tilemap = TilemapLayer.current

        if tilemap and tilemap.get_height() != 0 and tilemap.get_width() != 0:
            tile_size = tilemap.get_tileset().get_tile_size()

            map_width = tilemap.get_width() * tile_size
            map_height = tilemap.get_height() * tile_size

            small_tile = Size(tile_size * self.width() / map_width + 1,
                              tile_size * self.height() / map_height + 1)

            field = tilemap.get_field()

            # FIXME: No current tileset
            if False:
                for y in range(0, field.get_height()):
                    for x in range(0, field.get_width()):
                        tile = tilemap.get_tileset().create(field.at(x, y))
                        if tile:
                            gc.fill_rect(Rect(Point((x * tile_size) * self.width() / map_width,
                                                    (y * tile_size) * self.height() / map_height),
                                              Size(small_tile)),
                                         tile.get_color())
                        gc.flush()

            if self.minimap_surface:
                self.minimap_surface.draw(Rect(Point(0, 0), Size(self.width(), self.height())))

            # Draw cursor
            rect = self.parent.get_clip_rect()
            screen_rect = Rect(Point(rect.left * self.width() / map_width,
                                     rect.top * self.height() / map_height),
                               Size(rect.get_width() * self.width() / map_width,
                                    rect.get_height() * self.height() / map_height))
            gc.fill_rect(screen_rect, Color(255, 255, 0, 50))
            gc.draw_rect(screen_rect, Color(0, 0, 0))

        gc.pop_modelview()

    def update_minimap(self):
        # FIXME: This doesn't work all that well
        # tilemap = TilemapLayer.current
        pass
        # GRUMBEL
        # if tilemap:
        #     field = tilemap.get_field()
        #     buffer = PixelBuffer(tilemap.get_width(), tilemap.get_height())

        #     map_width = tilemap.get_width()
        #     map_height = tilemap.get_height()

        # FIXME: No Tileset.current
        #     buf = buffer.get_data()
        #     for y in range(0, map_height):
        #         for x in range(0, map_width):
        #             tile = tilemap.get_tileset().create(field.at(x, y))
        #             if tile:
        #                 buf[4 * (x + y * map_width) + 3] = tile.get_color().get_red()
        #                 buf[4 * (x + y * map_width) + 2] = tile.get_color().get_green()
        #                 buf[4 * (x + y * map_width) + 1] = tile.get_color().get_blue()
        #                 buf[4 * (x + y * map_width) + 0] = tile.get_color().get_alpha()
        #             else:
        #                 buf[4 * (x + y * map_width) + 0] = 0
        #                 buf[4 * (x + y * map_width) + 1] = 0
        #                 buf[4 * (x + y * map_width) + 2] = 0
        #                 buf[4 * (x + y * map_width) + 3] = 0
        #
        #    self.minimap_surface = Sprite(buffer)

    def mouseMoveEvent(self, event):
            # FIXME: This doesn't work all that well
        tilemap = TilemapLayer.current
        if tilemap:
            tile_size = tilemap.get_tileset().get_tile_size()
            map_width = tilemap.get_width() * tile_size
            map_height = tilemap.get_height() * tile_size

            if self.drag_active:
                self.parent.move_to(event.x() * map_width / self.width(),
                                    event.y() * map_height / self.height())
        self.repaint()

    def mousePressEvent(self, event):
        # FIXME: This doesn't work all that well
        tilemap = TilemapLayer.current
        if tilemap:
            tile_size = tilemap.get_tileset().get_tile_size()
            map_width = tilemap.get_width() * tile_size
            map_height = tilemap.get_height() * tile_size

            self.parent.move_to(event.x() * map_width / self.width(),
                                event.y() * map_height / self.height())
            self.drag_active = True
            self.grabMouse()

        self.repaint()

    def mouseReleaseEvent(self, event):
        tilemap = TilemapLayer.current
        if tilemap:
            self.drag_active = False
            self.releaseMouse()
        self.repaint()

# EOF #
