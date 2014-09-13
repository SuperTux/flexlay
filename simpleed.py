#!/usr/bin/env python3

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


import random
from flexlay.math import Size, Point, Rect
from flexlay.tools import WorkspaceMoveTool, ObjMapSelectTool, TileMapPaintTool
from flexlay import (Flexlay, PixelBuffer, Sprite, Tile, Tileset, EditorMap,
                     TilemapLayer, ObjectLayer, ObjMapRectObject, Color, TileBrush, ObjectBrush)


def main():
    flexlay = Flexlay()
    gui_manager = flexlay.create_gui_manager("Simple Editor")

    button_panel = gui_manager.create_button_panel(horizontal=True)

    def on_object_tool():
        workspace.set_tool(1, objtool)
        workspace.set_tool(2, workspace_move_tool)
        workspace.set_tool(3, Tool())
    button_panel.add_text("ObjectTool", on_object_tool)

    def on_tile_tool():
      workspace.set_tool(1, tilemap_paint_tool)
      workspace.set_tool(2, workspace_move_tool)
      workspace.set_tool(3, tilemap_paint_tool)
    button_panel.add_text("TileTool", on_tile_tool)

    def on_generic_dialog():
        dialog = gui_manager.create_generic_dialog("Generic Dialog")
        dialog.add_bool("An Bool:", True)
        dialog.add_int("An Int:", 5)
        dialog.add_float("An Int:", 5.0)
        dialog.add_string("An String:", "String")
        dialog.add_enum("An Enum:", ["String", "Foo", "bar"], 0)
        dialog.set_ok_callback(lambda: print(dialog.get_values()))
    button_panel.add_text("Generic Dialog", on_generic_dialog)

    menubar = gui_manager.create_menubar()

    file_menu = menubar.add_menu("&File")
    file_menu.add_item("Open...", lambda: print("Open"))
    file_menu.add_item("Save...", lambda: print("Save"))
    file_menu.add_item("Quit...", lambda: print("Quit"))
    
    view_menu = menubar.add_menu("&View")
    view_menu.add_item("Zoom In", lambda: print("Zoom In"))
    view_menu.add_item("Zoom Out", lambda: print("Zoom Out"))
    view_menu.add_item("Reset Zoom", lambda: print("Reset Zoom"))

    editor_map_component = gui_manager.create_editor_map_component()

    editormap = EditorMap()
    tileset = Tileset(32)
    tileset.add_tile(0, Tile(PixelBuffer.from_file("../data/images/icons16/resize1.png"), 
                             Sprite.from_file("../data/images/icons16/resize1.png")))
    tileset.add_tile(1, Tile(PixelBuffer.from_file("../data/images/icons16/resize_vert.png"),
                              Sprite.from_file("../data/images/icons16/resize_vert.png")))

    tilemap = TilemapLayer(tileset, 20, 10)
    TilemapLayer.current = tilemap
    tilemap.set_draw_grid(True)
    object_layer = ObjectLayer()
    editormap.add_layer(tilemap)
    editormap.add_layer(object_layer)

    for i in range(20):
        obj = ObjMapRectObject(Rect(Point(random.randint(0, 499), random.randint(0, 499)),
                                    Size(32, 32)), 
                               Color(0, 0, 255), None)
        object_layer.add_object(obj)

    workspace = editor_map_component.get_workspace()
    workspace.set_map(editormap)

    workspace_move_tool = WorkspaceMoveTool()
    objtool = ObjMapSelectTool()
    tilemap_paint_tool = TileMapPaintTool()
    brush = TileBrush(1, 1)
    brush.put(0, 0, 1)
    tilemap_paint_tool.set_brush(brush)
    workspace.set_tool(1, objtool)
    workspace.set_tool(2, workspace_move_tool)
    # workspace.set_tool(1, tilemap_paint_tool)
    # workspace.set_tool(3, tilemap_paint_tool)

    object_selector = gui_manager.create_object_selector(40, 40)
    object_selector.add_brush(ObjectBrush(Sprite.from_file("../data/images/icons16/resize1.png"), None))
    object_selector.add_brush(ObjectBrush(Sprite.from_file("../data/images/icons16/resize2.png"), None))
    tile_selector = gui_manager.create_tile_selector()
    tile_selector.set_tiles("All the tiles", tileset.get_tiles())
    tile_selector.set_tiles("All the tiles again", tileset.get_tiles())
    tile_selector.set_tiles("And again", tileset.get_tiles())

    print("Successs!")
    gui_manager.run()


if __name__ == "__main__":
    main()


# EOF #
