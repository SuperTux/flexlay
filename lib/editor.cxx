//  $Id: editor.cxx,v 1.11 2003/11/13 12:59:42 grumbel Exp $
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
#include <ClanLib/signals.h>
#include <ClanLib/display.h>
#include <ClanLib/gui.h>
#include <ClanLib/guistylesilver.h>

#include "gui_manager.hxx"
#include "editor.hxx"
#include "editor_map.hxx"
#include "tile_selector.hxx"
#include "tile_editor.hxx"
#include "globals.hxx"

Editor::Editor()
{
  manager      = new GUIManager();
}

Editor::~Editor()
{
  delete manager;
}

void
Editor::run()
{
  CL_Display::get_current_window()->show_cursor();

  std::cout << "Starting GUI manager busy loop..." << std::endl;
  manager->run();
  std::cout << "Starting GUI manager busy loop... done" << std::endl;
}

/* EOF */
