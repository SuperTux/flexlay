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


from flexlay import InputEvent, ToolContext
from flexlay.gui.editor_map_component import EditorMapComponent
from flexlay.tile_brush import TileBrush
from flexlay.graphic_context import GraphicContext
from flexlay.math import Rectf
from flexlay.tools.tool import Tool


class TileMapSelectTool(Tool):

    def __init__(self) -> None:
        super().__init__()
        assert ToolContext.current is not None
        self.selection = ToolContext.current.tile_selection
        self.creating_selection = False

    def draw(self, gc: GraphicContext) -> None:
        if self.selection.is_active():
            self.selection.draw(gc)

    def on_mouse_up(self, event: InputEvent) -> None:
        assert EditorMapComponent.current is not None
        assert ToolContext.current is not None
        assert ToolContext.current.tilemap_layer is not None
        assert event.mouse_pos is not None

        parent = EditorMapComponent.current

        if event.kind == InputEvent.MOUSE_LEFT:
            self.creating_selection = False
            parent.release_mouse()

            assert event.mouse_pos is not None
            assert ToolContext.current is not None
            self.selection.update(
                ToolContext.current.tilemap_layer.world2tile(parent.screen2world(event.mouse_pos.to_f())))

    def on_mouse_down(self, event: InputEvent) -> None:
        assert EditorMapComponent.current is not None
        assert ToolContext.current is not None
        assert ToolContext.current.tilemap_layer is not None
        assert event.mouse_pos is not None

        parent = EditorMapComponent.current

        if event.kind == InputEvent.MOUSE_LEFT:
            self.creating_selection = True
            parent.grab_mouse()
            tilemap = ToolContext.current.tilemap_layer
            assert event.mouse_pos is not None
            self.selection.start(tilemap, tilemap.world2tile(parent.screen2world(event.mouse_pos.to_f())))

        elif event.kind == InputEvent.MOUSE_RIGHT:
            if not self.creating_selection:
                self.selection.clear()

    def on_mouse_move(self, event: InputEvent) -> None:
        assert EditorMapComponent.current is not None
        assert ToolContext.current is not None
        assert ToolContext.current.tilemap_layer is not None
        assert event.mouse_pos is not None

        parent = EditorMapComponent.current

        if self.creating_selection:
            assert event.mouse_pos is not None
            self.selection.update(
                ToolContext.current.tilemap_layer.world2tile(parent.screen2world(event.mouse_pos.to_f())))

    def get_selection(self) -> TileBrush:
        assert ToolContext.current is not None
        assert ToolContext.current.tilemap_layer is not None
        tilemap = ToolContext.current.tilemap_layer
        return self.selection.get_brush(tilemap.field)

    def get_selection_rect(self) -> Rectf:
        assert self.selection is not None
        return self.selection.get_rect().to_f()


# EOF #
