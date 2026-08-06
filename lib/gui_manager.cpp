//  Flexlay - A Generic 2D Game Editor
//  Copyright (C) 2002 Ingo Ruhnke <grumbel@gmx.de>
//
//  This program is free software: you can redistribute it and/or modify
//  it under the terms of the GNU General Public License as published by
//  the Free Software Foundation, either version 3 of the License, or
//  (at your option) any later version.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//  GNU General Public License for more details.
//
//  You should have received a copy of the GNU General Public License
//  along with this program.  If not, see <http://www.gnu.org/licenses/>.

#include <ClanLib/gui.h>
#include <ClanLib/guistylesilver.h>
#include <ClanLib/core.h>
#include <ClanLib/Display/display.h>
#include <ClanLib/Display/display_window.h>

#include "globals.hpp"
#include "flexlay.hpp"
#include "gui_manager.hpp"

GUIManager* GUIManager::current_ = 0;

class GUIManagerImpl
{
public:
  std::stack<CL_Component*> components;

  CL_GUIManager*      manager;
  CL_StyleManager*    style;
  CL_ResourceManager* resources;
  CL_SlotContainer*   slot_container;
};

GUIManager::GUIManager()
  : impl(new GUIManagerImpl())
{
  std::cout << "Creating GUIManager: " << datadir + "/gui/gui.xml" << std::endl;
  impl->slot_container = new CL_SlotContainer();
  impl->resources = new CL_ResourceManager(datadir + "/gui/gui.xml");
  impl->style     = new CL_StyleManager_Silver(impl->resources);
  impl->manager   = new CL_GUIManager(impl->style);
  current_  = this;

  // OS window chrome close → leave the GUI run loop (File→Quit path).
  CL_DisplayWindow* win = CL_Display::get_current_window();
  if (win && impl->slot_container)
  {
    impl->slot_container->connect(win->sig_window_close(), this, &GUIManager::quit);
  }

  // Make the manager the first component on the stack
  push_component(impl->manager);
}

void
GUIManager::shutdown()
{
  if (!impl.get())
    return;

  if (current_ == this)
    current_ = 0;

  // Leave the run loop if it is still active.
  if (impl->manager)
  {
    try {
      impl->manager->quit();
    } catch (...) {
    }
  }

  // Do NOT delete CL_GUIManager / slot_container / style / resources.
  // ClanLib 1.0 menu node teardown is unsafe once Ruby/SWIG has held
  // parent-owned CL_Menu / CL_Component proxies — ~CL_MenuNode_Generic
  // segfaults on double-free or half-destroyed trees.  The process is
  // exiting; the OS reclaims the memory.
  while (!impl->components.empty())
    impl->components.pop();

  impl->manager = 0;
  impl->slot_container = 0;
  impl->style = 0;
  impl->resources = 0;
}

GUIManager::~GUIManager()
{
  // Always abandon; never call into ClanLib GUI destructors from here.
  // See shutdown() for rationale.
  if (current_ == this)
    current_ = 0;

  if (!impl.get())
    return;

  while (!impl->components.empty())
    impl->components.pop();

  impl->manager = 0;
  impl->slot_container = 0;
  impl->style = 0;
  impl->resources = 0;
}

void
GUIManager::draw()
{
  if (impl.get() && impl->manager && impl->manager->is_input_enabled())
    impl->manager->show();
}

void
GUIManager::update()
{
  // nothing to do
}

void
GUIManager::run()
{
  if (impl.get() && impl->manager)
    impl->manager->run();
}

CL_Component*
GUIManager::get_component()
{
  if (!impl.get() || impl->components.empty())
    return 0;
  return impl->components.top();
}

CL_SlotContainer*
GUIManager::get_slot_container()
{
  if (!impl.get())
    return 0;
  return impl->slot_container;
}

void
GUIManager::hide()
{
  if (impl.get() && impl->manager && impl->manager->is_input_enabled())
    impl->manager->disable_input();
}

void
GUIManager::show()
{
  if (impl.get() && impl->manager && !impl->manager->is_input_enabled())
    impl->manager->enable_input();
}

bool
GUIManager::is_visible()
{
  if (!impl.get() || !impl->manager)
    return false;
  return impl->manager->is_input_enabled();
}

void
GUIManager::quit()
{
  if (impl.get() && impl->manager)
    impl->manager->quit();
}

void
GUIManager::push_component(CL_Component* c)
{
  impl->components.push(c);
}

void
GUIManager::pop_component()
{
  impl->components.pop();
}

/* EOF */
