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


from flexlay import (Color, TileBrush, PaintCommand,
                     TilemapLayer, InputEvent, Workspace)
from ..gui.editor_map_component import EditorMapComponent
from ..gui.tile_selection import TileSelection
from flexlay.math import Point, Size, Rect


class TileMapPaintTool:

    current = None

    PAINTING_MODE = 0
    SELECTING_MODE = 1
    NONE_MODE = 2

    def __init__(self):
        TileMapPaintTool.current = self

        self.selection = TileSelection()
        self.last_draw = Point(-1, -1)
        self.brush = TileBrush(1, 1)
        self.brush.put(0, 0, 0)
        self.brush.set_opaque()
        self.current_tile = Point(0, 0)
        self.command = None
        self.mode = TileMapPaintTool.NONE_MODE

    def draw(self, gc):
        tilemap = TilemapLayer.current
        if not tilemap:
            return

        if self.mode == TileMapPaintTool.SELECTING_MODE:
            # if Keyboard.get_keycode(CL_KEY_LSHIFT):
            #    selection.draw(gc, Color(255,  128, 128, 100))
            # else:
            self.selection.draw(gc)

        else:
            tile_size = tilemap.get_tileset().get_tile_size()

            # Draw the brush:
            for y in range(0, self.brush.height):
                for x in range(0, self.brush.width):
                    tile = tilemap.get_tileset().create(self.brush.at(x, y))

                    if tile:
                        sprite = tile.get_sprite()
                        sprite.set_alpha(0.5)
                        sprite.draw((self.current_tile.x + x) * tile_size,
                                    (self.current_tile.y + y) * tile_size, gc)

                        gc.fill_rect(Rect(Point((self.current_tile.x + x) * tile_size,
                                                (self.current_tile.y + y) * tile_size),
                                          Size(tile_size, tile_size)),
                                     Color(255, 255, 255, 100))
                    elif self.brush.is_opaque():
                        gc.fill_rect(Rect(Point((self.current_tile.x + x) * tile_size,
                                                (self.current_tile.y + y) * tile_size),
                                          Size(tile_size, tile_size)),
                                     Color(255, 255, 255, 100))
                    else:
                        gc.fill_rect(Rect(Point((self.current_tile.x + x) * tile_size,
                                                (self.current_tile.y + y) * tile_size),
                                          Size(tile_size, tile_size)),
                                     Color(255, 255, 255, 50))

    def get_brush(self):
        return self.brush

    def on_mouse_down(self, event):
        tilemap = TilemapLayer.current

        if tilemap:
            parent = EditorMapComponent.current
            pos = tilemap.world2tile(parent.screen2world(event.mouse_pos))

            if self.mode == TileMapPaintTool.NONE_MODE:
                if event.kind == InputEvent.MOUSE_LEFT:
                    self.mode = TileMapPaintTool.PAINTING_MODE
                    parent.capture_mouse()
                    self.command = PaintCommand(tilemap, self.brush)
                    self.command.add_point(pos)
                    self.last_draw = pos

                elif event.kind == InputEvent.MOUSE_RIGHT:
                    self.mode = TileMapPaintTool.SELECTING
                    parent.capture_mouse()

                    self.selection.start(tilemap, pos)

    def on_mouse_move(self, event):
        tilemap = TilemapLayer.current
        if tilemap:
            parent = EditorMapComponent.current
            self.current_tile = tilemap.world2tile(parent.screen2world(event.mouse_pos))

            if self.mode == TileMapPaintTool.PAINTING_MODE:
                if ((event.mod & InputEvent.MOD_SHIFT) or
                    ((self.current_tile.x % self.brush.width) == (self.last_draw.x % self.brush.width) and
                     (self.current_tile.y % self.brush.height == (self.last_draw.y % self.brush.get_height())))):
                    self.command.add_point(self.current_tile)
                    self.last_draw = self.current_tile

            elif self.mode == TileMapPaintTool.SELECTING_MODE:
                self.selection.update(self.current_tile)

    def on_mouse_up(self, event):
        self.tilemap = TilemapLayer.current

        if self.tilemap:
            EditorMapComponent.current.get_workspace().get_map().modify()

            parent = EditorMapComponent.current
            self.current_tile = self.tilemap.world2tile(parent.screen2world(event.mouse_pos))

            if event.kind == InputEvent.MOUSE_LEFT:
                if self.mode == TileMapPaintTool.PAINTING_MODE:
                    parent.release_mouse()
                    self.mode = TileMapPaintTool.NONE_MODE

                    if ((event.mod & InputEvent.MOD_SHIFT) or
                        ((self.current_tile.x % self.brush.width) ==
                         (self.last_draw.x % self.brush.width) and
                         (self.current_tile.y % self.brush.height ==
                          (self.last_draw.y % self.brush.height)))):
                        self.command.add_point(self.current_tile)

                    Workspace.current.get_map().execute(self.command)
                    self.command = None

                    self.tilemap.draw_tile(self.brush, self.current_tile)
                    self.last_draw = Point(-1, -1)

            elif event.kind == InputEvent.MOUSE_RIGHT:
                if self.mode == TileMapPaintTool.SELECTING_MODE:
                    parent.release_mouse()
                    self.mode = TileMapPaintTool.NONE_MODE

                    self.selection.update(self.current_tile)
                    self.brush = self.selection.get_brush(self.tilemap.get_field())

                    if ((self.brush.width > 1 or self.brush.height > 1) and
                       (event.mod & InputEvent.MOD_SHIFT)):
                        self.brush.set_transparent()
                        self.brush.auto_crop()
                    else:
                        self.brush.set_opaque()

                    self.selection.clear()

    def set_brush(self, brush):
        self.brush = brush


# EOF #
