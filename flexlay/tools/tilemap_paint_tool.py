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


class TileMapPaintTool:

    current = None

    PAINTING_MODE = 0
    SELECTING_MODE = 1
    NONE_MODE = 2

    def __init__(self):
        TileMapPaintTool.current = self

        TileSelection selection
        TileBrush brush
        Point last_draw
        Point current_tile
        PaintCommand* command

        self.last_draw = Point(-1, -1)
        self.brush = TileBrush(1, 1)
        self.brush.at(0, 0) = 0
        self.brush.set_opaque()
        self.current_tile = Point(0,0) 
        self.command = 0
        self.mode = TileMapPaintToolImpl.NONE

    def draw(self, gc):
        tilemap = TilemapLayer.current()
        if not tilemap:
            return

        if self.mode == TileMapPaintToolImpl.SELECTING:
            #if Keyboard.get_keycode(CL_KEY_LSHIFT):
            #    selection.draw(gc, Color(255,  128, 128, 100))
            #else:
            selection.draw(gc)

        else:
            tile_size = tilemap.get_tileset().get_tile_size()

            # Draw the brush:
            for y in range(0, brush.get_height()):
                for x in range(0, brush.get_width()):
                    tile = tilemap.get_tileset().create(brush.at(x, y))

                    if tile:
                        sprite = tile->get_sprite()
                        sprite.set_alpha(0.5f)
                        sprite.draw((current_tile.x + x) * tile_size,
                                    (current_tile.y + y) * tile_size, gc)

                        gc.fill_rect(Rect(Point((current_tile.x + x) * tile_size,
                                                (current_tile.y + y) * tile_size),
                                          Size(tile_size, tile_size)),
                                     Color(255, 255, 255, 100))
                    elif brush.is_opaque():
                        gc.fill_rect(Rect(Point((current_tile.x + x) * tile_size,
                                                (current_tile.y + y) * tile_size),
                                          Size(tile_size, tile_size)),
                                     Color(255, 255, 255, 100))
                    else:
                          gc.fill_rect(Rect(Point((current_tile.x + x) * tile_size,
                                                  (current_tile.y + y) * tile_size),
                                            Size(tile_size, tile_size)),
                                       Color(255, 255, 255, 50))

    def get_brush(self):
            return self.brush

    def on_mouse_down(self, event):
        tilemap = TilemapLayer.current()

        if tilemap:
            parent = EditorMapComponent.current()
            pos = tilemap.world2tile(parent->screen2world(event.mouse_pos))

            if mode == TileMapPaintToolImpl.NONE:
                if event.kind == InputEvent.MOUSE_LEFT:
                    mode = TileMapPaintToolImpl.PAINTING
                    parent->capture_mouse()
                    command = new PaintCommand(tilemap, brush)
                    command->add_point(pos)
                    last_draw = pos

                elif event.kind == InputEvent.MOUSE_RIGHT:
                    mode = TileMapPaintToolImpl.SELECTING
                    parent->capture_mouse()

                    selection.start(tilemap, pos)

    def on_mouse_move(self, event):
        tilemap = TilemapLayer.current()
        if tilemap:
            parent = EditorMapComponent.current()
            current_tile = tilemap.world2tile(parent->screen2world(event.mouse_pos))

            if self.mode == PAINTING_MODE:
                if ((event.mod & InputEvent.MOD_SHIFT) ||
                    ((current_tile.x % brush.get_width()) == (last_draw.x % brush.get_width()) &&
                     (current_tile.y % brush.get_height() == (last_draw.y % brush.get_height())))):
                    command->add_point(current_tile)
                    last_draw = current_tile

            elif self.mode == SELECTING_MODE:
                selection.update(current_tile)

    def on_mouse_up(self, event)
      tilemap = TilemapLayer.current()

      if tilemap:
        EditorMapComponent.current()->get_workspace().get_map().modify()

        parent = EditorMapComponent.current()
        current_tile = tilemap.world2tile(parent->screen2world(event.mouse_pos))

        if event.kind == InputEvent.MOUSE_LEFT:
            if self.mode == PAINTING_MODE
              parent->release_mouse()
              mode = NONE

              if ((event.mod & InputEvent.MOD_SHIFT) ||
                  ((current_tile.x % brush.get_width()) == (last_draw.x % brush.get_width()) &&
                   (current_tile.y % brush.get_height() == (last_draw.y % brush.get_height())))):
                command->add_point(current_tile)

              Workspace.current().get_map().execute(command->to_command())
              command = None

              tilemap.draw_tile(brush, current_tile)
              last_draw = Point(-1, -1)

        elif event.kind == InputEvent.MOUSE_RIGHT:
            if mode == SELECTING:
                parent->release_mouse()
                mode = NONE

                selection.update(current_tile)
                brush = selection.get_brush(*tilemap.get_field())

                if ((brush.get_width() > 1 or brush.get_height() > 1)
                    and.mod & InputEvent.MOD_SHIFT)):
                    brush.set_transparent()
                    brush.auto_crop()
                else:
                    brush.set_opaque()

                selection.clear()

    def set_brush(self, brush):
        self.brush = brush


# EOF #
