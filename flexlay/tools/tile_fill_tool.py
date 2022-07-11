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
from flexlay.tool_context import ToolContext
from flexlay.tools.tool import Tool


class TileFillTool(Tool):
    current = None

    def __init__(self) -> None:
        super().__init__()

        TileFillTool.current = self

    def on_mouse_down(self, event):
        tilemap = ToolContext.current.tilemap_layer

        if tilemap and not tilemap.hidden:
            parent = EditorMapComponent.current
            pos = tilemap.world2tile(parent.screen2world(event.mouse_pos))
            tilemap.flood_fill_at(pos, ToolContext.current.tile_brush)
            # GRUMBEL: undo missing


# EOF #
