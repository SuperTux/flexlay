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
#include <ClanLib/Display/display.h>
#include <ClanLib/Display/keys.h>
#include "../windstille_level.hxx"
#include "../globals.hxx"
#include "../tile_factory.hxx"
#include "editor_names.hxx"
#include "editor_map.hxx"
#include "tool_manager.hxx"
#include "tilemap_tool.hxx"
#include "editor.hxx"
#include "editor_map_component.hxx"

EditorMapComponent* EditorMapComponent::current_ = 0; 

EditorMapComponent::EditorMapComponent(const CL_Rect& rect, CL_Component* parent)
  : CL_Component(rect, parent)
{
  current_ = this;

  slots.connect(sig_paint(),      this, &EditorMapComponent::draw);
  slots.connect(sig_mouse_up(),   this, &EditorMapComponent::mouse_up);
  slots.connect(sig_mouse_down(), this, &EditorMapComponent::mouse_down);
  slots.connect(sig_mouse_move(), this, &EditorMapComponent::mouse_move);

  trans_offset     = CL_Pointf(0,0);
  old_trans_offset = CL_Pointf(0,0);
  click_pos        = CL_Point(0,0);
  
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
      trans_offset.x = old_trans_offset.x - (click_pos.x - event.mouse_pos.x);
      trans_offset.y = old_trans_offset.y - (click_pos.y - event.mouse_pos.y);
          
      old_trans_offset = trans_offset;
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
      trans_offset.x = old_trans_offset.x - (click_pos.x - event.mouse_pos.x);
      trans_offset.y = old_trans_offset.y - (click_pos.y - event.mouse_pos.y);
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
      old_trans_offset = trans_offset;
      click_pos = event.mouse_pos;
      capture_mouse();
      break;
           
    case CL_MOUSE_WHEEL_UP:
      zoom_in();
      break;

    case CL_MOUSE_WHEEL_DOWN:
      zoom_out();
      break;
    }
}
  
void
EditorMapComponent::draw ()
{
  CL_Display::push_translate_offset(int(trans_offset.x), int(trans_offset.y));

  if (editor_map)
    editor_map->draw(this);

  if (1) // has_mouse_over()) FIXME: Seperate cursor and state here
    Editor::current()->get_tool_manager()->current_tool()->draw();
    
  CL_Display::flush();

  CL_Display::pop_translate_offset();
}

CL_Point
EditorMapComponent::screen2tile(const CL_Point& pos)
{
  // FIXME: Move this to EditorTilMap
  int x = int(pos.x - trans_offset.x)/TILE_SIZE;
  int y = int(pos.y - trans_offset.y)/TILE_SIZE;

  return CL_Point(pos.x - trans_offset.x < 0 ? x-1 : x,
                  pos.y - trans_offset.y < 0 ? y-1 : y); 
                  
}

CL_Point
EditorMapComponent::screen2world(const CL_Point& pos)
{
  int x = int(pos.x - trans_offset.x);
  int y = int(pos.y - trans_offset.y);

  return CL_Point(x, y);                  
}

void
EditorMapComponent::zoom_out()
{
  zoom_factor -= 1;
  std::cout << "Zoom: " << get_zoom() << std::endl;
}

void
EditorMapComponent::zoom_in()
{
  zoom_factor += 1;
  std::cout << "Zoom: " << get_zoom() << std::endl;
}

float
EditorMapComponent::get_zoom()
{
  if (zoom_factor > 0)
    return 1.0f * (zoom_factor + 1);
  else if (zoom_factor < 0)
    return 1.0f / (-zoom_factor + 1);
  else
    return 1.0f;
}

CL_Rect
EditorMapComponent::get_clip_rect()
{
  return CL_Rect(CL_Point(int(0 - trans_offset.x), int(0 - trans_offset.y)),
                 CL_Size(get_width(), 
                         get_height()));
}

void
EditorMapComponent::move_to(int x, int y)
{
  trans_offset = CL_Pointf(-x + get_width()/2,
                           -y + get_height()/2);
}

/* EOF */
