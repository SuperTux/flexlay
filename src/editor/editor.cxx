//  $Id: editor.cxx,v 1.6 2003/09/12 09:25:48 grumbel Exp $
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
#include <ClanLib/signals.h>
#include <ClanLib/display.h>
#include <ClanLib/guistylesilver.h>
#include <guile/gh.h>
#include "../globals.hxx"
#include "editor.hxx"
#include "editor_tilemap.hxx"
#include "tile_selector.hxx"

extern "C" void SWIG_init(void);

Editor* Editor::current_ = 0;

Editor::Editor()
{
  current_ = this;

  SWIG_init();

  slot_container = new CL_SlotContainer();
  resources = new CL_ResourceManager(datadir + "gui/gui.xml", false);
  style     = new CL_StyleManager_Silver(resources);
  manager   = new CL_GUIManager(style);

  push_component(manager);

  tilemap = new EditorTileMap(manager);
  //tilemap->load(datadir + "levels/level1.scm");

  popupmenu = new CL_PopupMenu(manager);
  menu_data  = new CL_MenuData(popupmenu);
  menu_data->insert_item("Hello World");
  menu_data->insert_item("Hello World2");
  menu_data->insert_item("Hello World3");

  gh_load ((datadir + "editor.scm").c_str());
}

void
Editor::load(const std::string& filename)
{
  tilemap->load(filename);
}

void
Editor::popup_menu()
{
  std::cout << "PopUP" << std::endl;
  popupmenu->popup(manager);
}

Editor::~Editor()
{
  delete manager;
  delete style;
  delete resources;
  delete slot_container;
}

void
Editor::run()
{
  manager->run();
}

/* EOF */
