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


from flexlay import TileBrush, TilemapLayer
from flexlay.commands.command import Command
from flexlay.math import Point, Rect


class PaintCommand(Command):

    def __init__(self, tilemap_layer, brush):
        super().__init__()

        self.points = []

        self.tilemap = tilemap_layer
        self.brush = brush

        # Copy of the field used to generate undo informations */
        self.undo_field = self.tilemap.field.copy()

        self.pos = Point(0, 0)
        self.redo_brush = None
        self.undo_brush = None

    def add_point(self, pos):
        # FIXME: undo_field is unneeded, should just record the overwritten color
        self.points.append(pos)
        self.tilemap.draw_tile_brush(self.brush, pos)

    def execute(self):
        assert self.points != []

        # Calc bounding rect
        rect = Rect(self.points[0].x,
                    self.points[0].y,
                    self.points[0].x + self.brush.width,
                    self.points[0].y + self.brush.height)

        for point in self.points:
            rect.left = min(rect.left, point.x)
            rect.top = min(rect.top, point.y)
            rect.right = max(rect.right, point.x + self.brush.width)
            rect.bottom = max(rect.bottom, point.y + self.brush.height)

        self.pos.x = rect.left
        self.pos.y = rect.top

        self.redo_brush = TileBrush.from_field(self.tilemap.field.copy(),
                                               rect.width, rect.height,
                                               -self.pos.x, -self.pos.y)

        # FIXME: undo_field is unneeded, should just record the overwritten color
        self.undo_brush = TileBrush.from_field(self.undo_field,
                                               rect.width, rect.height,
                                               -self.pos.x, -self.pos.y)

        self.redo_brush.set_opaque()
        self.undo_brush.set_opaque()

        self.undo_field.clear()

    def redo(self):
        TilemapLayer.draw_tiles(self.tilemap.field, self.redo_brush, self.pos)

    def undo(self):
        TilemapLayer.draw_tiles(self.tilemap.field, self.undo_brush, self.pos)


# EOF #
