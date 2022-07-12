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


import math

from flexlay.gui.editor_map_component import EditorMapComponent
from flexlay.math import Point, Pointf
from flexlay.input_event import InputEvent
from flexlay.graphic_context import GraphicContext
from flexlay.tools.tool import Tool


class WorkspaceMoveTool(Tool):

    def __init__(self) -> None:
        self.scrolling: bool = False
        self.click_pos: Point = Point(0, 0)
        self.old_trans_offset: Pointf = Pointf(0, 0)

    def on_mouse_down(self, event: InputEvent) -> None:
        assert EditorMapComponent.current is not None
        assert event.mouse_pos is not None

        self.scrolling = True
        self.old_trans_offset = EditorMapComponent.current.get_gc_state().get_pos()
        self.click_pos = event.mouse_pos
        EditorMapComponent.current.grab_mouse()

    def on_mouse_up(self, event: InputEvent) -> None:
        assert event.mouse_pos is not None
        assert EditorMapComponent.current is not None

        self.scrolling = False
        self.update(event)
        self.old_trans_offset = EditorMapComponent.current.get_gc_state().get_pos()
        EditorMapComponent.current.release_mouse()

    def on_mouse_move(self, event: InputEvent) -> None:
        if self.scrolling:
            self.update(event)

    def draw(self, gc: GraphicContext) -> None:
        pass

    def update(self, event: InputEvent) -> None:
        assert EditorMapComponent.current is not None

        gc_state = EditorMapComponent.current.get_gc_state()

        sa = math.sin(-gc_state.get_rotation() / 180.0 * math.pi)
        ca = math.cos(-gc_state.get_rotation() / 180.0 * math.pi)

        assert event.mouse_pos is not None
        dx = ca * (self.click_pos.x - event.mouse_pos.x) - sa * (self.click_pos.y - event.mouse_pos.y)
        dy = sa * (self.click_pos.x - event.mouse_pos.x) + ca * (self.click_pos.y - event.mouse_pos.y)

        gc_state.set_pos(Pointf(self.old_trans_offset.x + dx / EditorMapComponent.current.get_gc_state().get_zoom(),
                                self.old_trans_offset.y + dy / EditorMapComponent.current.get_gc_state().get_zoom()))


# EOF #
