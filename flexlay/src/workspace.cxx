//  $Id$
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

#include <ClanLib/Display/display.h>
#include <ClanLib/Display/keys.h>
#include "editor.hxx"
#include "editor_map.hxx"
#include "editor_map_component.hxx"
#include "editor_names.hxx"
#include "tilemap_tool.hxx"
#include "tileset.hxx"
#include "tool_manager.hxx"
#include "workspace.hxx"

Workspace* Workspace::current_ = 0;

WorkspaceItem::WorkspaceItem()
{
  pos = CL_Point(0, 0);
  editor_map = 0;
}

WorkspaceItem::WorkspaceItem(EditorMap* m, const CL_Point& p)
  : pos(p), editor_map(m)
{
}

Workspace::Workspace(int w, int h)
  : gc_state(w, h)
{
  current_ = this;
  scrolling = false;
  click_pos = CL_Point(0, 0);
  old_trans_offset = CL_Pointf(0,0);

  // FIXME: Dummy item
  items.push_back(new WorkspaceItem());
}

Workspace::~Workspace()
{
}

WorkspaceItem*
Workspace::get_current_item()
{
  return items.front();
}

void
Workspace::draw()
{
  gc_state.push();

  CL_Display::clear(CL_Color(100, 0, 100));
  for(Items::iterator i = items.begin(); i != items.end(); ++i)
    {
      if ((*i)->editor_map)
        {
          CL_Display::push_modelview();
          CL_Display::add_translate((*i)->pos.x, (*i)->pos.y);
      
          (*i)->editor_map->draw(EditorMapComponent::current());
      
          CL_Display::pop_modelview();
        }
    }

  if (1) // has_mouse_over()) FIXME: Seperate cursor and state here
    Editor::current()->get_tool_manager()->current_tool()->draw();
    
  CL_Display::flush();

  gc_state.pop();
}

void
Workspace::add_map(EditorMap* m, const CL_Point& p)
{
  items.push_back(new WorkspaceItem(m, p));  
}

void
Workspace::mouse_up(const CL_InputEvent& event)
{
  switch (event.id)
    {
    case CL_MOUSE_LEFT:
    case CL_MOUSE_RIGHT:
      Editor::current()->get_tool_manager()->current_tool()->on_mouse_up(event);
      break;

    case CL_MOUSE_MIDDLE:
      scrolling = false;
      gc_state.set_pos(CL_Pointf(old_trans_offset.x + (click_pos.x - event.mouse_pos.x) / gc_state.get_zoom(),
                                 old_trans_offset.y + (click_pos.y - event.mouse_pos.y) / gc_state.get_zoom()));
      old_trans_offset = gc_state.get_pos();
      EditorMapComponent::current()->release_mouse();
      break;
    }
}

void
Workspace::mouse_move(const CL_InputEvent& event)
{
  Editor::current()->get_tool_manager()->current_tool()->on_mouse_move(event);

  if (scrolling)
    {
      gc_state.set_pos(CL_Pointf(old_trans_offset.x + (click_pos.x - event.mouse_pos.x)/gc_state.get_zoom(),
                                 old_trans_offset.y + (click_pos.y - event.mouse_pos.y)/gc_state.get_zoom()));
    }
}

void
Workspace::mouse_down(const CL_InputEvent& event)
{
  switch (event.id)
    {
    case CL_MOUSE_LEFT:
    case CL_MOUSE_RIGHT:
      Editor::current()->get_tool_manager()->current_tool()->on_mouse_down(event);
      break;

    case CL_MOUSE_MIDDLE:
      scrolling = true;
      old_trans_offset = gc_state.get_pos();
      click_pos = event.mouse_pos;
      EditorMapComponent::current()->capture_mouse();
      break;
           
    case CL_MOUSE_WHEEL_UP:
      EditorMapComponent::current()->zoom_in(event.mouse_pos);
      break;

    case CL_MOUSE_WHEEL_DOWN:
      EditorMapComponent::current()->zoom_out(event.mouse_pos);
      break;
    }
}

EditorMap*
Workspace::get_current_map()
{
  return items.front()->editor_map;
}

void
Workspace::set_current_map(EditorMap* m)
{
  items.front()->editor_map = m;
}

/* EOF */
