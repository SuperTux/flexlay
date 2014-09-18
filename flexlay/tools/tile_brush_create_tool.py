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


from flexlay import (TilemapLayer, InputEvent)
from ..gui.editor_map_component import EditorMapComponent
from ..gui.tile_selection import TileSelection
from .tool import Tool
from ..color import Color
from ..tool_context import ToolContext


class TileBrushCreateTool(Tool):

    def __init__(self):
        super().__init__()
        self.is_active = False

        self.selection = TileSelection()
        self.shift_pressed = False

    def draw(self, gc):
        if self.is_active:
            tilemap = TilemapLayer.current
            if not tilemap:
                return

            if self.shift_pressed:
                self.selection.draw(gc, Color(255,  128, 128, 100))
            else:
                self.selection.draw(gc)

    def on_mouse_down(self, event):
        self.shift_pressed = event.mod & InputEvent.MOD_SHIFT

        tilemap = TilemapLayer.current

        if tilemap:
            parent = EditorMapComponent.current
            pos = tilemap.world2tile(parent.screen2world(event.mouse_pos))

            self.is_active = True
            self.grab_mouse()
            self.selection.start(tilemap, pos)

    def on_mouse_move(self, event):
        self.shift_pressed = event.mod & InputEvent.MOD_SHIFT

        tilemap = TilemapLayer.current
        if tilemap:
            parent = EditorMapComponent.current
            current_tile = tilemap.world2tile(parent.screen2world(event.mouse_pos))

            if self.is_active:
                self.selection.update(current_tile)

    def on_mouse_up(self, event):
        self.shift_pressed = event.mod & InputEvent.MOD_SHIFT

        tilemap = TilemapLayer.current

        if tilemap:
            EditorMapComponent.current.get_workspace().get_map().modify()

            parent = EditorMapComponent.current
            current_tile = tilemap.world2tile(parent.screen2world(event.mouse_pos))

            if self.is_active:
                self.release_mouse()
                self.is_active = False

                self.selection.update(current_tile)
                ToolContext.current.tile_brush = self.selection.get_brush(tilemap.get_field())

                if ToolContext.current.tile_brush.width > 1 or \
                   ToolContext.current.tile_brush.height > 1:
                    if self.shift_pressed:
                        ToolContext.current.tile_brush.set_opaque()
                    else:
                        ToolContext.current.tile_brush.set_transparent()
                        ToolContext.current.tile_brush.auto_crop()
                else:
                    ToolContext.current.tile_brush.set_opaque()

                self.selection.clear()


# EOF #
