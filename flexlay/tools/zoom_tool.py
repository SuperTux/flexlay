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

from flexlay import Color
from flexlay.gui.editor_map_component import EditorMapComponent
from flexlay.math import Rectf
from flexlay.tools.tool import Tool
from flexlay.input_event import InputEvent
from flexlay.graphic_context import GraphicContext


class ZoomTool(Tool):

    def __init__(self) -> None:
        super().__init__()

        self.zoom_rect: Optional[Rectf] = None

    def draw(self, gc: GraphicContext) -> None:
        if self.zoom_rect is not None:
            tmp = self.zoom_rect.copy()
            tmp.normalize()
            gc.fill_rect(tmp, Color(255, 255, 0, 50))
            gc.draw_rect(tmp, Color(255, 255, 0, 200))

    def on_mouse_up(self, event: InputEvent) -> None:
        assert EditorMapComponent.current is not None
        assert event.mouse_pos is not None

        parent = EditorMapComponent.current

        if self.zoom_rect is not None:
            parent.release_mouse()

            assert event.mouse_pos is not None
            pos = parent.screen2world(event.mouse_pos.to_f())
            self.zoom_rect.right = pos.x
            self.zoom_rect.bottom = pos.y
            self.zoom_rect.normalize()
            if self.zoom_rect.width > 10 and self.zoom_rect.height > 10:
                parent.zoom_to(self.zoom_rect)

            self.zoom_rect = None

    def on_mouse_down(self, event: InputEvent) -> None:
        assert EditorMapComponent.current is not None
        assert event.mouse_pos is not None

        parent = EditorMapComponent.current

        if self.zoom_rect is None:
            parent.grab_mouse()
            pos = parent.screen2world(event.mouse_pos.to_f())
            self.zoom_rect = Rectf(pos.x, pos.y,
                                   pos.x, pos.y)

    def on_mouse_move(self, event: InputEvent) -> None:
        assert EditorMapComponent.current is not None
        assert event.mouse_pos is not None

        parent = EditorMapComponent.current

        if self.zoom_rect is not None:
            pos = parent.screen2world(event.mouse_pos.to_f())
            self.zoom_rect.right = pos.x
            self.zoom_rect.bottom = pos.y


# EOF #
