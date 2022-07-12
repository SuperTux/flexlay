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

from flexlay import Workspace, Color, Layer
from flexlay.gui.editor_map_component import EditorMapComponent
from flexlay.math import Pointf
from flexlay.tools.tool import Tool
from flexlay.input_event import InputEvent
from flexlay.graphic_context import GraphicContext


class LayerMoveTool(Tool):

    def __init__(self) -> None:
        super().__init__()

        self.scrolling: bool = False
        self.click_pos: Pointf = Pointf(0, 0)
        self.old_trans_offset: Pointf = Pointf(0, 0)
        self.layer: Optional[Layer] = None

    def find_closed_layer(self, pos: Pointf) -> Layer:
        assert Workspace.current is not None

        layer: Optional[Layer] = None
        parent = Workspace.current.get_map()
        for i in range(0, parent.get_layer_count()):
            layer_i = parent.get_layer(i)
            assert layer_i is not None
            assert layer_i.get_bounding_rect() is not None
            if layer_i.get_bounding_rect().is_inside(pos):
                layer = layer_i
        assert layer is not None
        return layer

    def draw(self, gc: GraphicContext) -> None:
        assert Workspace.current is not None
        for i in range(0, Workspace.current.get_map().get_layer_count()):
            layer = Workspace.current.get_map().get_layer(i)
            assert layer is not None
            if layer.has_bounding_rect():
                rect = layer.get_bounding_rect()
                gc.draw_line(rect.left, rect.top, rect.right, rect.bottom,
                             Color(0, 255, 255))
                gc.draw_line(rect.left, rect.bottom, rect.right, rect.top,
                             Color(0, 255, 255))

    def on_mouse_up(self, event: InputEvent) -> None:
        if self.layer:
            self.scrolling = False
            self.update(event)
            assert EditorMapComponent.current is not None
            EditorMapComponent.current.release_mouse()
            self.layer = Layer()

    def on_mouse_down(self, event: InputEvent) -> None:
        assert EditorMapComponent.current is not None
        assert event.mouse_pos is not None

        parent = EditorMapComponent.current
        pos = parent.screen2world(event.mouse_pos.to_f())

        self.layer = self.find_closed_layer(pos)
        if self.layer:
            self.scrolling = True
            self.old_trans_offset = self.layer.get_pos()
            self.click_pos = pos
            EditorMapComponent.current.grab_mouse()

    def on_mouse_move(self, event: InputEvent) -> None:
        if self.layer and self.scrolling:
            self.update(event)

    def update(self, event: InputEvent) -> None:
        assert EditorMapComponent.current is not None
        assert event.mouse_pos is not None

        if self.layer:
            parent = EditorMapComponent.current
            pos = parent.screen2world(event.mouse_pos.to_f())
            self.layer.set_pos(self.old_trans_offset + (pos - self.click_pos))


# EOF #
