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

#include <iostream>
#include <ClanLib/Core/core_iostream.h>
#include <ClanLib/Display/display.h>
#include <ClanLib/Display/keys.h>
#include "tileset.hxx"
#include "editor_names.hxx"
#include "editor_map.hxx"
#include "tool_manager.hxx"
#include "tilemap_tool.hxx"
#include "editor.hxx"
#include "editor_map_component.hxx"

EditorMapComponent* EditorMapComponent::current_ = 0; 

EditorMapComponent::EditorMapComponent(const CL_Rect& rect, CL_Component* parent)
  : CL_Component(rect, parent),
    gc_state(this)
{
  current_ = this;

  slots.connect(sig_paint(),      this, &EditorMapComponent::draw);
  slots.connect(sig_mouse_up(),   this, &EditorMapComponent::mouse_up);
  slots.connect(sig_mouse_down(), this, &EditorMapComponent::mouse_down);
  slots.connect(sig_mouse_move(), this, &EditorMapComponent::mouse_move);

  zoom_factor = 0;

  editor_map = 0;

  scrolling = false;
}

EditorMapComponent::~EditorMapComponent()
{
}

void
EditorMapComponent::mouse_up(const CL_InputEvent& event)
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
      release_mouse();
      break;
    }
}

void
EditorMapComponent::mouse_move(const CL_InputEvent& event)
{
  Editor::current()->get_tool_manager()->current_tool()->on_mouse_move(event);

  if (scrolling)
    {
      gc_state.set_pos(CL_Pointf(old_trans_offset.x + (click_pos.x - event.mouse_pos.x)/gc_state.get_zoom(),
                                 old_trans_offset.y + (click_pos.y - event.mouse_pos.y)/gc_state.get_zoom()));
    }
}

void
EditorMapComponent::mouse_down(const CL_InputEvent& event)
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
      capture_mouse();
      break;
           
    case CL_MOUSE_WHEEL_UP:
      zoom_in(event.mouse_pos);
      break;

    case CL_MOUSE_WHEEL_DOWN:
      zoom_out(event.mouse_pos);
      break;
    }
}
  
void
EditorMapComponent::draw ()
{
  gc_state.push();

  CL_Display::clear(CL_Color(100, 0, 100));

  if (editor_map)
    editor_map->draw(this);

  if (1) // has_mouse_over()) FIXME: Seperate cursor and state here
    Editor::current()->get_tool_manager()->current_tool()->draw();
    
  CL_Display::flush();

  gc_state.pop();
}

CL_Point
EditorMapComponent::screen2world(const CL_Point& pos)
{
  CL_Pointf p = gc_state.screen2world(pos);
  return CL_Point((int)p.x, (int)p.y);
}

void
EditorMapComponent::zoom_out(CL_Point pos)
{
  gc_state.set_zoom(pos, gc_state.get_zoom()/1.25f);
  //zoom_factor -= 1;
  //std::cout << "Zoom: " << get_zoom() << std::endl;
}

void
EditorMapComponent::zoom_in(CL_Point pos)
{
  gc_state.set_zoom(pos, gc_state.get_zoom()*1.25f);
  //zoom_factor += 1;
  //std::cout << "Zoom: " << get_zoom() << std::endl;
}

void
EditorMapComponent::zoom_to(CL_Rect rect)
{
  gc_state.zoom_to(rect);
}

CL_Rect
EditorMapComponent::get_clip_rect()
{
  return gc_state.get_clip_rect();
}

void
EditorMapComponent::move_to(int x, int y)
{
  gc_state.set_pos(CL_Pointf(x, y));
}

/* EOF */
