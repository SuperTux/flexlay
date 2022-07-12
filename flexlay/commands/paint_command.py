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

from flexlay import TileBrush, TilemapLayer
from flexlay.commands.command import Command
from flexlay.math import Point, Rect
from flexlay.field import Field


class PaintCommand(Command):

    def __init__(self, tilemap_layer: TilemapLayer, brush: TileBrush) -> None:
        super().__init__()

        self.points: list[Point] = []

        self.tilemap = tilemap_layer
        self.brush = brush

        # Copy of the field used to generate undo informations */
        self.undo_field: Optional[Field] = self.tilemap.field.copy()

        self.pos: Point = Point(0, 0)
        self.redo_brush: Optional[TileBrush] = None
        self.undo_brush: Optional[TileBrush] = None

    def add_point(self, pos: Point) -> None:
        # FIXME: undo_field is unneeded, should just record the overwritten color
        self.points.append(pos)
        self.tilemap.draw_tile_brush(self.brush, pos)

    def execute(self) -> None:
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
        assert self.undo_field is not None
        self.undo_brush = TileBrush.from_field(self.undo_field,
                                               rect.width, rect.height,
                                               -self.pos.x, -self.pos.y)

        self.redo_brush.set_opaque()
        self.undo_brush.set_opaque()

        self.undo_field = None

    def redo(self) -> None:
        assert self.redo_brush is not None
        TilemapLayer.draw_tiles(self.tilemap.field, self.redo_brush, self.pos)

    def undo(self) -> None:
        assert self.undo_brush is not None
        TilemapLayer.draw_tiles(self.tilemap.field, self.undo_brush, self.pos)


# EOF #
