//  $Id$
//
//  Pingus - A free Lemmings clone
//  Copyright (C) 2002 Ingo Ruhnke <grumbel@gmx.de>
//
//  This program is free software; you can redistribute it and/or
//  modify it under the terms of the GNU General Public License
//  as published by the Free Software Foundation; either version 2
//  of the License, or (at your option) any later version.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//  GNU General Public License for more details.
//
//  You should have received a copy of the GNU General Public License
//  along with this program; if not, write to the Free Software
//  Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.

#include <iostream>
#include <string>
#include <ClanLib/core.h>
#include <ClanLib/gui.h>
#include "editor.hxx"
#include "workspace.hxx"
#include "tileset.hxx"
#include "editor_map_component.hxx"
#include "gui_manager.hxx"
#include "layer.hxx"
#include "tilemap_layer.hxx"
#include "flexlay.hxx"

int main()
{
  Flexlay flexlay;
  flexlay.init();

  Editor editor;

  GUIManager* gui = editor.get_gui_manager();

  EditorMap m;
  Tileset tileset(32);

    std::cout << "\nTilemapLayer: start" << std::endl;
    TilemapLayer tilemap(tileset, 20, 10);
    std::cout << "bound1: " << tilemap.get_bounding_rect() << std::endl;
    Layer l = tilemap.to_layer();
    Layer l2 = l;
    std::cout << "bound2: " << l2.get_bounding_rect() << std::endl;
    std::cout << "adding layer " << std::endl;
    m.add_layer(l2);
    std::cout << "layer added" << std::endl;
    std::cout << "bound4: " << l.get_bounding_rect() << std::endl;

  std::cout << "TilemapLayer: end\n" << std::endl;


  EditorMapComponent editor_map(CL_Rect(0, 0, 799, 599), gui->get_component());
  Workspace workspace(799, 599);
  editor_map.set_workspace(workspace);
  workspace.set_current_map(m);
 
  CL_Button* button = new CL_Button(CL_Rect(CL_Point(50, 50), 
                                            CL_Size(100, 25)),
                                    "Hello World", gui->get_component());

  gui->run();
 
  flexlay.deinit();
}

/* EOF */
