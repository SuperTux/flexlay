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


from flexlay import (Color, PaintCommand,
                     InputEvent, Workspace)
from flexlay.gui.editor_map_component import EditorMapComponent
from flexlay.math import Point, Size, Rect
from flexlay.tool_context import ToolContext
from .tool import Tool


class TilePaintTool(Tool):
    current = None

    def __init__(self):
        super().__init__()

        TilePaintTool.current = self

        self.last_draw = Point(-1, -1)

        self.current_tile = Point(0, 0)
        self.command = None
        self.is_active = False

    def draw(self, gc):
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

                    gc.fill_rect(Rect(Point((self.current_tile.x + x) * tile_size,
                                            (self.current_tile.y + y) * tile_size),
                                      Size(tile_size, tile_size)),
                                 Color(255, 255, 255, 100))
                elif ToolContext.current.tile_brush.is_opaque():
                    gc.fill_rect(Rect(Point((self.current_tile.x + x) * tile_size,
                                            (self.current_tile.y + y) * tile_size),
                                      Size(tile_size, tile_size)),
                                 Color(255, 255, 255, 100))
                else:
                    gc.fill_rect(Rect(Point((self.current_tile.x + x) * tile_size,
                                            (self.current_tile.y + y) * tile_size),
                                      Size(tile_size, tile_size)),
                                 Color(255, 255, 255, 50))

    def on_mouse_down(self, event):
        tilemap = ToolContext.current.tilemap_layer
        if tilemap:
            parent = EditorMapComponent.current
            pos = tilemap.world2tile(parent.screen2world(event.mouse_pos))

            self.is_active = True
            self.grab_mouse()
            self.command = PaintCommand(ToolContext.current.tilemap_layer, ToolContext.current.tile_brush)
            self.command.add_point(pos)
            self.last_draw = pos

    def on_mouse_move(self, event):
        tilemap = ToolContext.current.tilemap_layer
        if tilemap:
            parent = EditorMapComponent.current
            self.current_tile = tilemap.world2tile(parent.screen2world(event.mouse_pos))

            if self.is_active:
                brush = ToolContext.current.tile_brush
                if ((event.mod & InputEvent.MOD_SHIFT) or
                        ((self.current_tile.x % brush.width) == (self.last_draw.x % brush.width) and
                                 (self.current_tile.y % brush.height) == (self.last_draw.y % brush.height))):
                    self.command.add_point(self.current_tile)
                    self.last_draw = self.current_tile

    def on_mouse_up(self, event):
        tilemap = ToolContext.current.tilemap_layer
        if tilemap:
            EditorMapComponent.current.get_workspace().get_map().modify()

            parent = EditorMapComponent.current
            self.current_tile = tilemap.world2tile(parent.screen2world(event.mouse_pos))

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
