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


from flexlay.gui.editor_map_component import EditorMapComponent
from flexlay.math import Pointf
from flexlay.tools.tool import Tool
from flexlay.input_event import InputEvent


class Zoom2Tool(Tool):

    def __init__(self) -> None:
        super().__init__()

        self.active: bool = False
        self.click_pos = Pointf(0, 0)
        self.old_zoom: float = 0.0

    def on_mouse_up(self, event: InputEvent) -> None:
        self.active = False

    def on_mouse_down(self, event: InputEvent) -> None:
        assert EditorMapComponent.current is not None
        assert event.mouse_pos is not None

        self.active = True
        self.click_pos = event.mouse_pos.to_f()

        gc = EditorMapComponent.current.get_gc_state()
        self.old_zoom = gc.get_zoom()

    def on_mouse_move(self, event: InputEvent) -> None:
        assert EditorMapComponent.current is not None
        assert event.mouse_pos is not None

        if self.active:
            gc = EditorMapComponent.current.get_gc_state()
            zoom_pos = Pointf(gc.width / 2, gc.height / 2)

            factor = (event.mouse_pos.y - self.click_pos.y) / 20.0
            if factor > 0:
                gc.set_zoom(self.old_zoom * pow(1.25, factor), zoom_pos)
            elif factor < 0:
                gc.set_zoom(self.old_zoom / pow(1.25, -factor), zoom_pos)
            else:
                gc.set_zoom(self.old_zoom, zoom_pos)


# EOF #
