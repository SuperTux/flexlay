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

from flexlay.color import Color
from flexlay.input_event import InputEvent
from flexlay.workspace import Workspace
from flexlay.commands.paint_command import PaintCommand
from flexlay.gui.editor_map_component import EditorMapComponent
from flexlay.math import Point, Pointf, Rectf, Sizef
from flexlay.tool_context import ToolContext
from flexlay.tools.tool import Tool
from flexlay.graphic_context import GraphicContext


class TilePaintTool(Tool):

    current: Optional['TilePaintTool'] = None

    def __init__(self) -> None:
        super().__init__()

        TilePaintTool.current = self

        self.last_draw = Point(-1, -1)

        self.current_tile = Point(0, 0)
        self.command: Optional[PaintCommand] = None
        self.is_active = False

    def draw(self, gc: GraphicContext) -> None:
        assert ToolContext.current is not None
        tilemap = ToolContext.current.tilemap_layer
        if not tilemap:
            return

        tile_size = tilemap.get_tileset().get_tile_size()

        # Draw the brush:
        for y in range(0, ToolContext.current.tile_brush.height):
            for x in range(0, ToolContext.current.tile_brush.width):
                tile = tilemap.get_tileset().create(ToolContext.current.tile_brush.at(x, y))

                if tile:
                    sprite = tile.get_sprite()
                    sprite.set_alpha(0.5)
                    sprite.draw((self.current_tile.x + x) * tile_size,
                                (self.current_tile.y + y) * tile_size, gc)

                    gc.fill_rect(Rectf.from_ps(Pointf((self.current_tile.x + x) * tile_size,
                                                      (self.current_tile.y + y) * tile_size),
                                               Sizef(tile_size, tile_size)),
                                 Color(255, 255, 255, 100))
                elif ToolContext.current.tile_brush.is_opaque():
                    gc.fill_rect(Rectf.from_ps(Pointf((self.current_tile.x + x) * tile_size,
                                                      (self.current_tile.y + y) * tile_size),
                                               Sizef(tile_size, tile_size)),
                                 Color(255, 255, 255, 100))
                else:
                    gc.fill_rect(Rectf.from_ps(Pointf((self.current_tile.x + x) * tile_size,
                                                      (self.current_tile.y + y) * tile_size),
                                               Sizef(tile_size, tile_size)),
                                 Color(255, 255, 255, 50))

    def on_mouse_down(self, event: InputEvent) -> None:
        assert EditorMapComponent.current is not None
        assert ToolContext.current is not None
        assert ToolContext.current.tilemap_layer is not None
        assert event.mouse_pos is not None

        tilemap = ToolContext.current.tilemap_layer
        if tilemap and not tilemap.hidden:
            parent = EditorMapComponent.current
            pos = tilemap.world2tile(parent.screen2world(event.mouse_pos.to_f()))

            self.is_active = True
            self.grab_mouse()
            self.command = PaintCommand(ToolContext.current.tilemap_layer, ToolContext.current.tile_brush)
            self.command.add_point(pos)
            self.last_draw = pos

    def on_mouse_move(self, event: InputEvent) -> None:
        assert EditorMapComponent.current is not None
        assert ToolContext.current is not None
        assert event.mouse_pos is not None

        tilemap = ToolContext.current.tilemap_layer
        if tilemap:
            parent = EditorMapComponent.current
            self.current_tile = tilemap.world2tile(parent.screen2world(event.mouse_pos.to_f()))

            if self.is_active and not tilemap.hidden:
                assert self.command is not None

                brush = ToolContext.current.tile_brush
                if ((event.mod & InputEvent.MOD_SHIFT) or
                    ((self.current_tile.x % brush.width) == (self.last_draw.x % brush.width) and
                     (self.current_tile.y % brush.height) == (self.last_draw.y % brush.height))):
                    self.command.add_point(self.current_tile)
                    self.last_draw = self.current_tile

    def on_mouse_up(self, event: InputEvent) -> None:
        assert EditorMapComponent.current is not None
        assert ToolContext.current is not None
        assert event.mouse_pos is not None
        assert self.command is not None
        assert Workspace.current is not None

        tilemap = ToolContext.current.tilemap_layer
        if tilemap and not tilemap.hidden:
            EditorMapComponent.current.get_workspace().get_map().modify()

            parent = EditorMapComponent.current
            self.current_tile = tilemap.world2tile(parent.screen2world(event.mouse_pos.to_f()))

            if self.is_active:
                self.release_mouse()
                self.is_active = False

                if ((event.mod & InputEvent.MOD_SHIFT) or
                    ((self.current_tile.x % ToolContext.current.tile_brush.width) ==
                     (self.last_draw.x % ToolContext.current.tile_brush.width) and
                     (self.current_tile.y % ToolContext.current.tile_brush.height ==
                      (self.last_draw.y % ToolContext.current.tile_brush.height)))):
                    self.command.add_point(self.current_tile)

                Workspace.current.get_map().execute(self.command)
                self.command = None

                tilemap.draw_tile_brush(ToolContext.current.tile_brush, self.current_tile)
                self.last_draw = Point(-1, -1)


# EOF #
