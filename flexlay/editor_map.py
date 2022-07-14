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


from typing import cast, Any, Optional

from flexlay.color import Color
from flexlay.layer import Layer
from flexlay.tilemap_layer import TilemapLayer
from flexlay.objmap_tilemap_object import ObjMapTilemapObject
from flexlay.math import Rect, Rectf
from flexlay.util.signal import Signal
from flexlay.commands.command import Command
from flexlay.graphic_context import GraphicContext
from flexlay.object_layer import ObjectLayer


class EditorMap:

    def __init__(self) -> None:
        self.background_color: Color = Color(100, 80, 100)
        self.foreground_color: Color = Color(255, 80, 255)
        self.modified: bool = False
        self.serial: int = 0
        self._has_bounding_rect: bool = False
        self.bounding_rect = Rectf(0, 0, 0, 0)
        self.layers: list[Layer] = []
        self.redo_stack: list[Command] = []
        self.undo_stack: list[Command] = []
        # Index in undo_stack + redo_stack, all with index <= are saved.
        self.save_pointer: int = 0
        self.sig_change: Signal = Signal()
        self.metadata: Any = None
        self.draw_grid: bool = False

    def add_layer(self, layer: Layer, pos: int = -1) -> None:
        assert pos == -1 or (pos >= 0 and pos < len(self.layers))
        assert isinstance(layer, Layer)

        if pos == -1:  # insert at last pos
            self.layers.append(layer)
        else:
            self.layers.insert(pos, layer)

        self.serial += 1

    def remove_tilemap_layer(self, tilemap_layer: TilemapLayer) -> ObjMapTilemapObject:
        index = 0

        layer = self.layers[0]
        assert isinstance(layer, ObjectLayer)
        object_layer = cast(ObjectLayer, layer)

        for object in object_layer.objects:
            if isinstance(object, ObjMapTilemapObject):
                layer = object.tilemap_layer
                if isinstance(layer, TilemapLayer):
                    if layer is tilemap_layer:
                        del object_layer.objects[index]
                        return object
            index += 1

        raise RuntimeError("TilemapLayer not found for removal")

    def add_tilemap_layer(self, tilemap_layer: TilemapLayer) -> None:
        layer = self.layers[0]
        assert isinstance(layer, ObjectLayer)
        object_layer = cast(ObjectLayer, layer)

        object_layer.objects.append(ObjMapTilemapObject(tilemap_layer, None))

    def draw_background(self, gc: GraphicContext) -> None:
        bounding_rect = self.get_bounding_rect()
        if bounding_rect != Rect(0, 0, 0, 0):
            gc.fill_rect(bounding_rect, self.background_color)

    def draw_foreground(self, gc: GraphicContext) -> None:
        bounding_rect = self.get_bounding_rect()
        if bounding_rect != Rect(0, 0, 0, 0):
            if self.draw_grid:
                rect = gc.get_clip_rect().to_i()

                start_x = rect.left // 32
                start_y = rect.top // 32
                end_x = rect.right // 32 + 1
                end_y = rect.bottom // 32 + 1
                tile_size = 32

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

            # bounding rect
            gc.draw_rect(bounding_rect, self.foreground_color)

    def draw(self, gc: GraphicContext) -> None:
        self.draw_background(gc)
        for layer in self.layers:
            layer.draw(gc)
        self.draw_foreground(gc)

    def is_modified(self) -> bool:
        return self.modified

    def set_unmodified(self) -> None:
        self.modified = False

    def modify(self) -> None:
        self.modified = True
        self.serial += 1

    def get_serial(self) -> int:
        return self.serial

    def get_layer_count(self) -> int:
        return len(self.layers)

    def get_layer(self, i: int) -> Optional[Layer]:
        if i >= 0 and i < len(self.layers):
            return self.layers[i]
        else:
            return None

    def get_tilemap_layers(self) -> list[TilemapLayer]:
        """Return a list containing only tilemaps in editormap"""
        tilemap_layers = []
        # As TilemapLayers are used by ObjMapTilemapObjects,
        # which are stored in the objects array in an ObjectLayer. (!)
        layer = self.layers[0]
        assert isinstance(layer, ObjectLayer)
        object_layer = cast(ObjectLayer, layer)
        for object in object_layer.objects:
            if isinstance(object, ObjMapTilemapObject):
                layer = object.tilemap_layer
                if isinstance(layer, TilemapLayer):
                    tilemap_layers.append(layer)
        return tilemap_layers

    def has_bounding_rect(self) -> bool:
        return self._has_bounding_rect

    def set_bounding_rect(self, rect: Rectf) -> None:
        if rect != Rect(0, 0, 0, 0):
            self._has_bounding_rect = True
            self.bounding_rect = rect
        else:
            self._has_bounding_rect = False
            self.bounding_rect = rect

    def get_bounding_rect(self) -> Rectf:
        if self._has_bounding_rect:
            return self.bounding_rect
        else:
            init = False
            rect = Rectf(0, 0, 0, 0)

            for layer in self.layers:
                if layer.has_bounding_rect():
                    if not init:
                        rect = layer.get_bounding_rect()
                        init = True
                    else:
                        other = layer.get_bounding_rect()
                        rect.top = min(rect.top, other.top)
                        rect.bottom = max(rect.bottom, other.bottom)
                        rect.left = min(rect.left, other.left)
                        rect.right = max(rect.right, other.right)

            return rect

    def set_background_color(self, color: Color) -> None:
        self.background_color = color

    def get_background_color(self) -> Color:
        return self.background_color

    def execute(self, command: Command) -> None:
        self.redo_stack.clear()
        command.execute()
        self.undo_stack.append(command)
        self.sig_change()

    def undo(self) -> None:
        if self.undo_stack:
            command = self.undo_stack[-1]
            self.undo_stack.pop()
            command.undo()
            self.redo_stack.append(command)
            self.sig_change()

    def redo(self) -> None:
        if self.redo_stack:
            command = self.redo_stack[-1]
            self.redo_stack.pop()
            command.redo()
            self.undo_stack.append(command)
            self.sig_change()

    def undo_stack_size(self) -> int:
        return len(self.undo_stack)

    def redo_stack_size(self) -> int:
        return len(self.redo_stack)


# EOF #
