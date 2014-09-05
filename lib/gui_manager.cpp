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

#include <ClanLib/core.h>
#include <ClanLib/gui.h>
#include <ClanLib/guistylesilver.h>

#include "globals.hpp"
#include "gui/button_panel.hpp"
#include "gui/editor_map_component.hpp"
#include "gui/file_dialog.hpp"
#include "gui/generic_dialog.hpp"
#include "gui/menubar.hpp"
#include "gui/minimap.hpp"
#include "gui_manager.hpp"

GUIManager* GUIManager::current_ = 0;

class GUIManagerImpl
{
public:
  CL_GUIManager*      manager;
  CL_StyleManager*    style;
  CL_ResourceManager* resources;
};

GUIManager::GUIManager()
  : impl(new GUIManagerImpl())
{
  std::cout << "Creating GUIManager: " << datadir + "/gui/gui.xml" << std::endl;
  impl->resources = new CL_ResourceManager(datadir + "/gui/gui.xml");
  impl->style     = new CL_StyleManager_Silver(impl->resources);
  impl->manager   = new CL_GUIManager(impl->style);
  current_  = this;
}

GUIManager::~GUIManager()
{
  delete impl->manager;
  //delete style; FIXME: Memory hole?!
  //delete resources;  FIXME: Memory hole?!
}

void
GUIManager::draw()
{
  if (impl->manager->is_input_enabled())
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
  impl->manager->run();
}

CL_Component*
GUIManager::get_component()
{
  return impl->manager;
}

void
GUIManager::hide()
{
  if (impl->manager->is_input_enabled())
    impl->manager->disable_input();
}

void
GUIManager::show()
{
  if (!impl->manager->is_input_enabled())
    impl->manager->enable_input();
}

bool
GUIManager::is_visible()
{
  return impl->manager->is_input_enabled();
}

void
GUIManager::quit()
{
  impl->manager->quit();
}

Menubar*
GUIManager::create_menubar()
{
  return new Menubar(get_component());
}

ButtonPanel*
GUIManager::create_button_panel(const Rect& rect, bool horizontal)
{
  return new ButtonPanel(rect, horizontal, get_component());
}

GenericDialog*
GUIManager::create_generic_dialog(const std::string& title)
{
  return new GenericDialog(title, get_component());
}

EditorMapComponent*
GUIManager::create_editor_map_component(const Rect& rect)
{
  return new EditorMapComponent(rect.to_cl(), get_component());
}

Minimap*
GUIManager::create_minimap(EditorMapComponent* parent, const Rect& rect)
{
  return new Minimap(parent, rect, get_component());
}

FileDialog*
GUIManager::create_filedialog(const std::string& titel,
                              const std::string& ok_label, const std::string& cancel_label)
{
  return new FileDialog(titel, ok_label, cancel_label, get_component());
}

/* EOF */
