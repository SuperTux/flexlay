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


class Workspace:
    current = None

    def __init__(self):
        Workspace.current = self

        self.tools = {}
        self.editor_map = None

    def draw(self, gc):
        if self.editor_map:
            self.editor_map.draw(gc)

            # FIXME: Only draw active tool?!
            for tool in self.tools.values():
                tool.draw(gc)

    def mouse_up(self, event):
        tool = self.tools.get(event.kind)
        if tool is not None:
            tool.on_mouse_up(event)

    def mouse_move(self, event):
        for tool in self.tools.values():
            tool.on_mouse_move(event)

    def mouse_down(self, event):
        tool = self.tools.get(event.kind)
        if tool is not None:
            tool.on_mouse_down(event)

    def key_up(self, event):
        tool = self.tools.get(event.kind)
        if tool is not None:
            tool.on_mouse_up(event)

    def key_down(self, event):
        tool = self.tools.get(event.kind)
        if tool is not None:
            tool.on_mouse_down(event)
        else:
            print("Workspace:", event.kind)

    def get_map(self):
        return self.editor_map

    def set_map(self, editor_map):
        self.editor_map = editor_map

    def set_tool(self, button, tool):
        from flexlay.tools import Tool
        if tool is None:
            tool = Tool()
        self.tools[button] = tool

# EOF #
