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
#include "../globals.hxx"
#include "../scripting/editor.hxx"
#include "editor.hxx"
#include "editor_map.hxx"
#include "minimap.hxx"

Minimap::Minimap(const CL_Point& pos, const CL_Size& size, CL_Component* parent)
  : CL_Component(CL_Rect(pos, size), parent)
{
  slots.push_back(sig_paint().connect(this, &Minimap::draw));
  slots.push_back(sig_mouse_move().connect(this, &Minimap::mouse_move));
  slots.push_back(sig_mouse_down().connect(this, &Minimap::mouse_down));
  slots.push_back(sig_mouse_up().connect(this, &Minimap::mouse_up));

  drag_active = false;
}

void
Minimap::draw()
{
  if (0)
    {
      CL_Display::fill_rect(CL_Rect(CL_Point(0, 0),
                                    CL_Size(get_width(),
                                            get_height())),
                            CL_Color(255, 0, 255));
    }

  int map_width  = map_get_width()  * TILE_SIZE;
  int map_height = map_get_height() * TILE_SIZE;

  CL_Rect rect = Editor::current()->get_map()->get_clip_rect();

  CL_Rect screen_rect(CL_Point(rect.left  * get_width()  / map_width,
                               rect.top   * get_height() / map_height),
                      CL_Size(rect.get_width() * get_width() /map_width,
                              rect.get_height()* get_height()/map_height));

  CL_Display::fill_rect(screen_rect,
                        CL_Color(255, 255, 0, 50));
  CL_Display::draw_rect(screen_rect,
                        CL_Color(0, 0, 0));
}

void
Minimap::mouse_move(const CL_InputEvent& event)
{
  int map_width  = map_get_width()  * TILE_SIZE;
  int map_height = map_get_height() * TILE_SIZE;

  if (drag_active)
    Editor::current()->get_map()->move_to(event.mouse_pos.x * map_width / get_width(),
                                          event.mouse_pos.y * map_height / get_height());
}

void
Minimap::mouse_down(const CL_InputEvent& event)
{
  int map_width  = map_get_width()  * TILE_SIZE;
  int map_height = map_get_height() * TILE_SIZE;

  Editor::current()->get_map()->move_to(event.mouse_pos.x * map_width / get_width(),
                                        event.mouse_pos.y * map_height / get_height());
  drag_active = true;
  capture_mouse();
}

void
Minimap::mouse_up  (const CL_InputEvent& event)
{
  drag_active = false;
  release_mouse();
}

/* EOF */
