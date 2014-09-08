// Flexlay - A Generic 2D Game Editor
// Copyright (C) 2002 Ingo Ruhnke <grumbel@gmail.com>
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <http://www.gnu.org/licenses/>.

#include "editor_map.hpp"
#include "flexlay.hpp"
#include "gui/button_panel.hpp"
#include "gui/editor_map_component.hpp"
#include "gui/generic_dialog.hpp"
#include "gui/menubar.hpp"
#include "gui_manager.hpp"
#include "math/rect.hpp"
#include "object_layer.hpp"
#include "objmap_rect_object.hpp"
#include "tile.hpp"
#include "tile_brush.hpp"
#include "tilemap_layer.hpp"
#include "tileset.hpp"
#include "tools/objmap_select_tool.hpp"
#include "tools/tilemap_paint_tool.hpp"
#include "tools/workspace_move_tool.hpp"
#include "workspace.hpp"

int main()
{
  Flexlay flexlay;
  flexlay.init();

  GUIManager gui;

  EditorMap m(true);
  Tileset tileset(32);
  tileset.add_tile(0, new Tile(PixelBuffer::from_file("resize1"),
                               Sprite(PixelBuffer::from_file("resize1"))));
  tileset.add_tile(1, new Tile(PixelBuffer::from_file("resize_vert"),
                               Sprite(PixelBuffer::from_file("resize_vert"))));

  TilemapLayer tilemap(tileset, 20, 10);
  tilemap.set_draw_grid(true);
  ObjectLayer object_layer;
  m.add_layer(tilemap.to_layer());
  m.add_layer(object_layer.to_layer());

  for(int i = 0; i < 20; ++i)
  {
    ObjMapRectObject obj(Rect(Point(rand() % 500, rand() % 500), Size(32, 32)), Color(0, 0, 255), {});
    object_layer.add_object(obj.to_object());
  }

  ObjectLayer::set_current(object_layer);

  TilemapLayer::set_current(tilemap);

  ButtonPanel* buttons = gui.create_button_panel(true);
  
  Menubar* menubar = gui.create_menubar();
  Menu file_menu = menubar->add_menu("&File");
  file_menu.add_item("Open...", []{ std::cout << "Open" << std::endl; });
  file_menu.add_item("Save...", []{ std::cout << "Save" << std::endl; });
  file_menu.add_item("Quit...", []{ std::cout << "Quit" << std::endl; });

  Menu view_menu = menubar->add_menu("&View");
  view_menu.add_item("Zoom In", []{  std::cout << "Zoom In" << std::endl; });
  view_menu.add_item("Zoom Out", []{ std::cout << "Zoom Out" << std::endl; });
  view_menu.add_item("Reset Zoom", []{ std::cout << "Reset Zoom" << std::endl; });

  EditorMapComponent* editor_map = gui.create_editor_map_component();
  Workspace workspace = editor_map->get_workspace();
  workspace.set_map(m);

  WorkspaceMoveTool workspace_move_tool;
  ObjMapSelectTool objtool;
  TileMapPaintTool tilemap_paint_tool;
  TileBrush brush(1, 1);
  brush.at(0, 0) = 1;
  tilemap_paint_tool.set_brush(brush);
  workspace.set_tool(1, objtool.to_tool());
  workspace.set_tool(2, workspace_move_tool.to_tool());
  workspace.set_tool(1, tilemap_paint_tool.to_tool());
  workspace.set_tool(3, tilemap_paint_tool.to_tool());

  buttons->add_text("ObjectTool", [&]{
      workspace.set_tool(1, objtool.to_tool());
      workspace.set_tool(2, workspace_move_tool.to_tool());
      workspace.set_tool(3, Tool());
    });
  buttons->add_text("TileTool", [&]{
      workspace.set_tool(1, tilemap_paint_tool.to_tool());
      workspace.set_tool(2, workspace_move_tool.to_tool());
      workspace.set_tool(3, tilemap_paint_tool.to_tool());
    });

  GenericDialog* dialog = gui.create_generic_dialog("Generic Dialog");
  dialog->add_int("An Int:", 5);

  gui.run();

  flexlay.deinit();
}

/* EOF */
