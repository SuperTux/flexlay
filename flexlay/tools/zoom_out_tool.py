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
from flexlay.input_event import InputEvent
from flexlay.tools.tool import Tool


class ZoomOutTool(Tool):

    def __init__(self) -> None:
        super().__init__()

    def on_mouse_down(self, event: InputEvent) -> None:
        assert EditorMapComponent.current is not None
        assert event.mouse_pos is not None

        parent = EditorMapComponent.current
        if event.kind == InputEvent.MOUSE_RIGHT:
            parent.zoom_out(event.mouse_pos.to_f())


# EOF #
