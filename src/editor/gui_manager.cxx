//  $Id: gui_manager.cxx,v 1.3 2003/11/05 12:41:37 grumbel Exp $
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

#include <ClanLib/gui.h>
#include <ClanLib/guistylesilver.h>
#include <ClanLib/core.h>

#include "scripting/gui.hxx"
#include "gui_manager.hxx"
#include "globals.hxx"

GUIManager* GUIManager::current_ = 0;

GUIManager::GUIManager()
{
  slot_container = new CL_SlotContainer();
  resources = new CL_ResourceManager(datadir + "gui/gui.xml", false);
  style     = new CL_StyleManager_Silver(resources);
  manager   = new CL_GUIManager(style);

  current_ = this;

  // Make the manager the first component on the stack
  gui_push_component(manager);
}

GUIManager::~GUIManager()
{
  gui_pop_component();

  delete manager;
  //delete style; FIXME: Memory hole?!
  //delete resources;  FIXME: Memory hole?!
  delete slot_container;
}
  
void
GUIManager::draw()
{
  if (manager->is_input_enabled())
    manager->show();
}

void
GUIManager::update()
{
  // nothing to do
}

void
GUIManager::run()
{
  manager->run();
}

CL_Component* 
GUIManager::get_component()
{
  return components.top();
}

CL_SlotContainer*
GUIManager::get_slot_container()
{
  return slot_container;
}

void
GUIManager::hide()
{
  if (manager->is_input_enabled())
    manager->disable_input();
}

void
GUIManager::show()
{
  if (!manager->is_input_enabled())
    manager->enable_input();
}

bool
GUIManager::is_visible()
{
  return manager->is_input_enabled();
}

/* EOF */
