//  $Id$
//
//  Flexlay - A Generic 2D Game Editor
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
#include "editor_map.hxx"

int main()
{
  Flexlay flexlay;
  flexlay.init();

  Editor editor;

  GUIManager* gui = editor.get_gui_manager();

  new CL_Button(CL_Rect(CL_Point(50, 50), 
                        CL_Size(100, 25)),
                "Hello World", gui->get_component());

  EditorMap m;
  Tileset tileset(32);
  TilemapLayer tilemap(tileset, 20, 10);
  
  m.add_layer(tilemap.to_layer());

  TilemapLayer::set_current(tilemap);

  EditorMapComponent editor_map(CL_Rect(0, 0, 799, 599), gui->get_component());
  Workspace workspace(799, 599);
  editor_map.set_workspace(workspace);
  workspace.set_map(m);
 
  new CL_Button(CL_Rect(CL_Point(50, 150), 
                        CL_Size(100, 25)),
                "Quit", gui->get_component());

  gui->run();
 
  flexlay.deinit();
}

/* EOF */
