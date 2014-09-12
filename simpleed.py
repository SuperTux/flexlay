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
                     TilemapLayer, ObjectLayer, ObjMapRectObject, Color, TileBrush)


def main():
    flexlay = Flexlay()
    gui_manager = flexlay.create_gui_manager("Simple Editor")

    button_panel = gui_manager.create_button_panel(horizontal=True)
    button_panel.add_text("ClickMe", None)
    button_panel.add_text("ClickYou", None)

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
    tileset.add_tile(0, Tile(PixelBuffer.from_file("resize1"), 
                             Sprite.from_file("resize1")))
    tileset.add_tile(1, Tile(PixelBuffer.from_file("resize_vert"),
                              Sprite.from_file("resize_vert")))

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

    print("Successs!")
    gui_manager.run()
  

  # buttons->add_text("ObjectTool", [&]{
  #     workspace.set_tool(1, objtool.to_tool())
  #     workspace.set_tool(2, workspace_move_tool.to_tool())
  #     workspace.set_tool(3, Tool())
  #   })
  # buttons->add_text("TileTool", [&]{
  #     workspace.set_tool(1, tilemap_paint_tool.to_tool())
  #     workspace.set_tool(2, workspace_move_tool.to_tool())
  #     workspace.set_tool(3, tilemap_paint_tool.to_tool())
  #   })

  # GenericDialog* dialog = gui->create_generic_dialog("Generic Dialog")
  # dialog->add_int("An Int:", 5)

  # gui->run()

if __name__ == "__main__":
    main()


# EOF #
