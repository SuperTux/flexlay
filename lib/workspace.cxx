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
#include <ClanLib/Display/display.h>
#include <ClanLib/Display/display_window.h>
#include <ClanLib/Display/keys.h>
#include "editor.hxx"
#include "editor_map.hxx"
#include "editor_map_component.hxx"
#include "editor_names.hxx"
#include "tool.hxx"
#include "workspace_move_tool.hxx"
#include "tileset.hxx"
#include "workspace.hxx"

Workspace Workspace::current_;

class WorkspaceImpl
{
public:
  GraphicContextState gc_state;
  EditorMap editor_map;

  Tool tool;
  Tool move_tool;
};

Workspace::Workspace()
{
}

Workspace::Workspace(int w, int h)
  : impl(new WorkspaceImpl())
{
  current_ = *this;
  
  impl->move_tool = WorkspaceMoveTool(*this).to_tool();
  impl->gc_state  = GraphicContextState(w, h);
}

void
Workspace::draw()
{
  impl->gc_state.push();

  CL_Display::clear(CL_Color(100, 0, 100));

  impl->editor_map.draw_gui(CL_Display::get_current_window()->get_gc());
  impl->editor_map.draw(EditorMapComponent::current(), CL_Display::get_current_window()->get_gc());
  
  if (1) // has_mouse_over()) FIXME: Seperate cursor and state here
    impl->tool.draw();
    
  CL_Display::flush();

  impl->gc_state.pop();
}

void
Workspace::mouse_up(const CL_InputEvent& event)
{
  switch (event.id)
    {
    case CL_MOUSE_LEFT:
    case CL_MOUSE_RIGHT:
      impl->tool.on_mouse_up(event);
      break;

    case CL_MOUSE_MIDDLE:
      impl->move_tool.on_mouse_up(event);
      break;
    }
}

void
Workspace::mouse_move(const CL_InputEvent& event)
{
  impl->tool.on_mouse_move(event);
  impl->move_tool.on_mouse_move(event);
}

void
Workspace::mouse_down(const CL_InputEvent& event)
{
  switch (event.id)
    {
    case CL_MOUSE_LEFT:
    case CL_MOUSE_RIGHT:
      impl->tool.on_mouse_down(event);
      break;

    case CL_MOUSE_MIDDLE:
      impl->move_tool.on_mouse_down(event);
      break;
      
    case CL_MOUSE_WHEEL_UP:
      EditorMapComponent::current()->zoom_in(event.mouse_pos);
      break;

    case CL_MOUSE_WHEEL_DOWN:
      EditorMapComponent::current()->zoom_out(event.mouse_pos);
      break;
    }
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

GraphicContextState&
Workspace::get_gc_state()
{
  return impl->gc_state;
}

void
Workspace::set_tool(const Tool& tool)
{
  impl->tool = tool;
}

/* EOF */
