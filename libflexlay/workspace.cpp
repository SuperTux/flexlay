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

#include "workspace.hpp"

#include <assert.h>
#include <iostream>
#include <map>

#include "editor_map.hpp"
#include "editor_names.hpp"
#include "graphic_context.hpp"
#include "gui/editor_map_component.hpp"
#include "input_event.hpp"
#include "tileset.hpp"
#include "tools/tool.hpp"

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
Workspace::draw(GraphicContext& gc)
{
  if (impl->editor_map)
  {
    assert(impl.get());

    impl->editor_map.draw_gui(gc);
    impl->editor_map.draw(gc);

    // FIXME: Only draw active tool?!
    for(WorkspaceImpl::Tools::iterator it = impl->tools.begin();
        it != impl->tools.end(); ++it)
    {
      it->second.draw(gc);
    }

    gc.flush();
  }
}

void
Workspace::mouse_up(const InputEvent& event)
{
  assert(impl.get());

  WorkspaceImpl::Tools::iterator it = impl->tools.find(event.id);
  if (it != impl->tools.end())
    it->second.on_mouse_up(event);
}

void
Workspace::mouse_move(const InputEvent& event)
{
  assert(impl.get());

  for(WorkspaceImpl::Tools::iterator it = impl->tools.begin();
      it != impl->tools.end(); ++it)
  {
    it->second.on_mouse_move(event);
  }
}

void
Workspace::mouse_down(const InputEvent& event)
{
  assert(impl.get());

  WorkspaceImpl::Tools::iterator it = impl->tools.find(event.id);
  if (it != impl->tools.end())
    it->second.on_mouse_down(event);

  switch (event.id)
  {
    case InputEvent::MOUSE_WHEEL_UP:
      EditorMapComponent::current()->zoom_in(event.mouse_pos);
      break;

    case InputEvent::MOUSE_WHEEL_DOWN:
      EditorMapComponent::current()->zoom_out(event.mouse_pos);
      break;

   default:
      break;
  }
}

void
Workspace::key_up(const InputEvent& event)
{
  assert(impl.get());

  WorkspaceImpl::Tools::iterator it = impl->tools.find(event.id);
  if (it != impl->tools.end())
    it->second.on_mouse_up(event);
}

void
Workspace::key_down(const InputEvent& event)
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
