//  $Id: editor.cxx,v 1.11 2003/11/13 12:59:42 grumbel Exp $
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
#include <ClanLib/gui.h>
#include <ClanLib/guistylesilver.h>
#include <guile/gh.h>

#include "gui_manager.hxx"
#include "globals.hxx"
#include "editor.hxx"
#include "editor_map.hxx"
#include "tile_selector.hxx"
#include "tile_editor.hxx"

Editor* Editor::current_ = 0;

Editor::Editor()
{
  current_ = this;

  manager = new GUIManager();

  map_comp = new EditorMap(manager->get_component());
  //tilemap = new EditorTileMap(manager->get_component());
  //objmap  = new EditorObjMap(manager->get_component());

  gh_load ((datadir + "editor.scm").c_str());
}

Editor::~Editor()
{
  delete manager;
}

void
Editor::load(const std::string& filename)
{
  // FIXME: implement me
  //tilemap->load(filename);
}

void
Editor::run()
{
  CL_Display::get_current_window()->show_cursor();
  manager->run();
}

/* EOF */
