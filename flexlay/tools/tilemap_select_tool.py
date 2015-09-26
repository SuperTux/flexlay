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
from ..gui.editor_map_component import EditorMapComponent


class TileMapSelectTool:

    def __init__(self):
        self.selection = ToolContext.current.tile_selection
        self.creating_selection = False

    def draw(self, gc):
        if self.selection.is_active():
            self.selection.draw(gc)

    def on_mouse_up(self, event):
        parent = EditorMapComponent.current

        if event.kind == InputEvent.MOUSE_LEFT:
            self.creating_selection = False
            parent.release_mouse()

            self.selection.update(ToolContext.current.tilemap_layer.world2tile(parent.screen2world(event.mouse_pos)))

    def on_mouse_down(self, event):
        parent = EditorMapComponent.current

        if event.kind == InputEvent.MOUSE_LEFT:
            self.creating_selection = True
            parent.grab_mouse()
            tilemap = ToolContext.current.tilemap_layer
            self.selection.start(tilemap, tilemap.world2tile(parent.screen2world(event.mouse_pos)))

        elif event.kind == InputEvent.MOUSE_RIGHT:
            if not self.creating_selection:
                self.selection.clear()

    def on_mouse_move(self, event):
        parent = EditorMapComponent.current

        if self.creating_selection:
            self.selection.update(ToolContext.current.tilemap_layer.world2tile(parent.screen2world(event.mouse_pos)))

    def get_selection(self):
        tilemap = ToolContext.current.tilemap_layer
        return self.selection.get_brush(tilemap.field)

    def get_selection_rect(self):
        return self.selection.get_rect()


# EOF #
