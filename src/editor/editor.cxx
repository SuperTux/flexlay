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
#include "command.hxx"
#include "editor.hxx"
#include "editor_map.hxx"
#include "tile_selector.hxx"
#include "tile_editor.hxx"

Editor* Editor::current_ = 0;

Editor::Editor()
{
  current_ = this;

  manager = new GUIManager();

  gh_load ((datadir + "editor.scm").c_str());
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

void
Editor::execute(Command* command)
{
  for(std::vector<Command*>::iterator i = redo_stack.begin(); 
      i != redo_stack.end(); ++i)
    delete (*i);
  redo_stack.clear();

  command->execute();
  undo_stack.push_back(command);
}

void
Editor::undo()
{
  if (!undo_stack.empty())
    {
      Command* command = undo_stack.back();
      undo_stack.pop_back();
      command->undo();
      redo_stack.push_back(command);
    }
}

void
Editor::redo()
{
  if (!redo_stack.empty())
    {
      Command* command = redo_stack.back();
      redo_stack.pop_back();
      command->redo();
      undo_stack.push_back(command);
    }
}

/* EOF */
