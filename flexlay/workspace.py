# Flexlay - A Generic 2D Game Editor
# Copyright (C) 2014 Ingo Ruhnke <grumbel@gmail.com>
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.


from typing import Any, Optional

import logging

from flexlay.editor_map import EditorMap
from flexlay.graphic_context import GraphicContext
from flexlay.input_event import InputEvent
from flexlay.tools.tool import Tool

# from supertux.sector import Sector


class Workspace:

    current = None

    def __init__(self) -> None:
        Workspace.current = self

        self.tools: dict[int, 'Tool'] = {}
        self.editor_map: Optional[EditorMap] = None
        self.current_sector: Optional[Any] = None

    def draw(self, gc: GraphicContext) -> None:
        if self.editor_map:
            self.editor_map.draw(gc)

            # FIXME: Only draw active tool?!
            for tool in self.tools.values():
                tool.draw(gc)

    def mouse_up(self, event: InputEvent) -> None:
        tool = self.tools.get(event.kind)
        if tool is not None:
            tool.on_mouse_up(event)

    def mouse_move(self, event: InputEvent) -> None:
        for tool in self.tools.values():
            tool.on_mouse_move(event)

    def mouse_down(self, event: InputEvent) -> None:
        tool = self.tools.get(event.kind)
        if tool is not None:
            tool.on_mouse_down(event)

    def key_up(self, event: InputEvent) -> None:
        tool = self.tools.get(event.kind)
        if tool is not None:
            tool.on_mouse_up(event)

    def key_down(self, event: InputEvent) -> None:
        tool = self.tools.get(event.kind)
        if tool is not None:
            tool.on_mouse_down(event)
        else:
            logging.info("Workspace: " + str(event.kind))

    def get_map(self) -> EditorMap:
        assert self.editor_map is not None
        return self.editor_map

    def set_map(self, editor_map: EditorMap) -> None:
        self.editor_map = editor_map

    def set_tool(self, button: int, tool: Optional[Tool]) -> None:
        from flexlay.tools import Tool
        if tool is None:
            tool = Tool()
        self.tools[button] = tool


# EOF #
