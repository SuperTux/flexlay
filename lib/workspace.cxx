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
#include <map>
#include <ClanLib/Display/display.h>
#include <ClanLib/Display/display_window.h>
#include <ClanLib/Display/keys.h>
#include "editor_map.hxx"
#include "editor_map_component.hxx"
#include "editor_names.hxx"
#include "tools/tool.hxx"
#include "tileset.hxx"
#include "workspace.hxx"

Workspace Workspace::current_(false);

class WorkspaceImpl
{
public:
  EditorMap editor_map;

  typedef std::map<int, Tool> Tools;
  Tools tools;
};

Workspace::Workspace(bool create)
  : impl(0)
{
  if (create)
    {
      impl = new WorkspaceImpl();
      current_ = *this;
      std::cout << "Workspace()" << std::endl;
    }
}

void
Workspace::draw()
{
  CL_Display::clear(CL_Color(100, 0, 100));

  impl->editor_map.draw_gui(CL_Display::get_current_window()->get_gc());
  impl->editor_map.draw(EditorMapComponent::current(), CL_Display::get_current_window()->get_gc());
  
  // FIXME: Only draw active tool?!
  for(WorkspaceImpl::Tools::iterator it = impl->tools.begin();
      it != impl->tools.end(); ++it)
    it->second.draw();
    
  CL_Display::flush();
}

void
Workspace::mouse_up(const CL_InputEvent& event)
{
  WorkspaceImpl::Tools::iterator it = impl->tools.find(event.id);
  if (it != impl->tools.end())
    it->second.on_mouse_up(event);
}

void
Workspace::mouse_move(const CL_InputEvent& event)
{
  for(WorkspaceImpl::Tools::iterator it = impl->tools.begin();
      it != impl->tools.end(); ++it)
    {
      it->second.on_mouse_move(event);
    }
}

void
Workspace::mouse_down(const CL_InputEvent& event)
{
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
  WorkspaceImpl::Tools::iterator it = impl->tools.find(event.id);
  if (it != impl->tools.end())
    it->second.on_mouse_up(event);
}

void
Workspace::key_down(const CL_InputEvent& event)
{
  std::cout << "Workspace: " << event.id << std::endl;
  WorkspaceImpl::Tools::iterator it = impl->tools.find(event.id);
  if (it != impl->tools.end())
    it->second.on_mouse_down(event);
}

EditorMap
Workspace::get_map()
{
  return impl->editor_map;
}

void
Workspace::set_map(const EditorMap& m)
{
  impl->editor_map = m;
}

void
Workspace::set_tool(int button, const Tool& tool)
{
  impl->tools[button] = tool;
}

/* EOF */
