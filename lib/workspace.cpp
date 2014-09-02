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

#include "workspace.hpp"

#include <ClanLib/Display/display.h>
#include <ClanLib/Display/display_window.h>
#include <ClanLib/Display/keys.h>
#include <assert.h>
#include <iostream>
#include <map>

#include "gui/editor_map_component.hpp"
#include "editor_map.hpp"
#include "editor_names.hpp"
#include "tools/tool.hpp"
#include "tileset.hpp"

Workspace Workspace::current_(false);

class WorkspaceImpl
{
public:
  EditorMap editor_map;

  typedef std::map<int, Tool> Tools;
  Tools tools;
};

Workspace::Workspace(bool create) :
  impl()
{
  if (create)
  {
    impl.reset(new WorkspaceImpl());
    current_ = *this;
    std::cout << "Workspace()" << std::endl;
  }
}

void
Workspace::draw(const GraphicContextState& state, CL_GraphicContext* gc)
{
  assert(impl.get());

  CL_Display::clear(CL_Color(100, 0, 100));

  impl->editor_map.draw_gui(CL_Display::get_current_window()->get_gc());
  impl->editor_map.draw(EditorMapComponent::current()->get_gc_state(), CL_Display::get_current_window()->get_gc());

  // FIXME: Only draw active tool?!
  for(WorkspaceImpl::Tools::iterator it = impl->tools.begin();
      it != impl->tools.end(); ++it)
    it->second.draw();

  CL_Display::flush();
}

void
Workspace::mouse_up(const CL_InputEvent& event)
{
  assert(impl.get());

  WorkspaceImpl::Tools::iterator it = impl->tools.find(event.id);
  if (it != impl->tools.end())
    it->second.on_mouse_up(event);
}

void
Workspace::mouse_move(const CL_InputEvent& event)
{
  assert(impl.get());

  for(WorkspaceImpl::Tools::iterator it = impl->tools.begin();
      it != impl->tools.end(); ++it)
  {
    it->second.on_mouse_move(event);
  }
}

void
Workspace::mouse_down(const CL_InputEvent& event)
{
  assert(impl.get());

  WorkspaceImpl::Tools::iterator it = impl->tools.find(event.id);
  if (it != impl->tools.end())
    it->second.on_mouse_down(event);

  switch (event.id)
  {
    case CL_MOUSE_WHEEL_UP:
      EditorMapComponent::current()->zoom_in(event.mouse_pos);
      break;

    case CL_MOUSE_WHEEL_DOWN:
      EditorMapComponent::current()->zoom_out(event.mouse_pos);
      break;
  }
}

void
Workspace::key_up(const CL_InputEvent& event)
{
  assert(impl.get());

  WorkspaceImpl::Tools::iterator it = impl->tools.find(event.id);
  if (it != impl->tools.end())
    it->second.on_mouse_up(event);
}

void
Workspace::key_down(const CL_InputEvent& event)
{
  assert(impl.get());

  WorkspaceImpl::Tools::iterator it = impl->tools.find(event.id);
  if (it != impl->tools.end())
    it->second.on_mouse_down(event);
  else
    std::cout << "Workspace: " << event.id << std::endl;
}

EditorMap
Workspace::get_map()
{
  assert(impl.get());

  return impl->editor_map;
}

void
Workspace::set_map(const EditorMap& m)
{
  assert(impl.get());

  impl->editor_map = m;
  std::cout << "Workspace:set_map" << std::endl;
}

void
Workspace::set_tool(int button, const Tool& tool)
{
  assert(impl.get());

  impl->tools[button] = tool;
}

/* EOF */
