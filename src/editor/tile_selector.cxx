//  $Id: tile_selector.cxx,v 1.2 2003/09/10 18:56:03 grumbel Exp $
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
#include <ClanLib/display.h>
#include "../globals.hxx"
#include "../tile_factory.hxx"
#include "../tile.hxx"
#include "scripting.hxx"
#include "tile_selector.hxx"

TileSelector::TileSelector(int width, int height, CL_Component* parent)
  : CL_Component(CL_Rect(CL_Point(0,0), CL_Size(width * TILE_SIZE, height * TILE_SIZE)), parent),
    width(width), height(height)
{
  index = 0;

  slots.connect(sig_paint(),      this, &TileSelector::draw);
  slots.connect(sig_mouse_move(), this, &TileSelector::mouse_move);
  slots.connect(sig_mouse_down(), this, &TileSelector::mouse_down);
  slots.connect(sig_mouse_up  (), this, &TileSelector::mouse_up);
 
  mouse_over_tile = -1;
  scrolling = false;
  offset = 0;
}

void
TileSelector::mouse_up(const CL_InputEvent& event)
{
  if (event.id == CL_MOUSE_MIDDLE)
    {
      scrolling = false;
      release_mouse();
    }
}

void
TileSelector::mouse_down(const CL_InputEvent& event)
{
  if (event.id == CL_MOUSE_LEFT)
    {
      editor_set_brush_tile(mouse_over_tile);
    }
  else if (event.id == CL_MOUSE_MIDDLE)
    {
      scrolling = true;
      mouse_pos = event.mouse_pos;
      old_offset = offset;
      capture_mouse();
    }

}

void
TileSelector::mouse_move(const CL_InputEvent& event)
{
  int x = event.mouse_pos.x/(TILE_SIZE/2);
  int y = (event.mouse_pos.y+offset)/(TILE_SIZE/2);

  mouse_over_tile = y * width + x;

  if (scrolling)
    {
      offset = old_offset + (mouse_pos.y - event.mouse_pos.y);
      if (offset < 0)
        offset = 0;
    }
}

void 
TileSelector::draw()
{
  CL_Display::push_translate_offset(0, -offset);
  for(int y = 0; y < height; ++y)
    for(int x = 0; x < width; ++x)
      {
        int i = width * y + x;
        Tile* tile = TileFactory::current()->create(i);
        if (tile)
          {
            CL_Sprite sprite = tile->sur;
            sprite.set_scale(0.5f, 0.5f);
            sprite.draw(x * TILE_SIZE/2, y * TILE_SIZE/2);
            CL_Display::draw_rect(CL_Rect(CL_Point(x * TILE_SIZE/2, y * TILE_SIZE/2),
                                          CL_Size(TILE_SIZE/2, TILE_SIZE/2)),
                                  CL_Color(0,0,0,128));
          }

        if (i == editor_get_brush_tile())
          {
            CL_Display::fill_rect(CL_Rect(CL_Point(x * TILE_SIZE/2, y * TILE_SIZE/2),
                                          CL_Size(TILE_SIZE/2, TILE_SIZE/2)),
                                  CL_Color(0,0,255, 100));
          }
        else if (mouse_over_tile == i && has_mouse_over())
          {
            CL_Display::fill_rect(CL_Rect(CL_Point(x * TILE_SIZE/2, y * TILE_SIZE/2),
                                          CL_Size(TILE_SIZE/2, TILE_SIZE/2)),
                                  CL_Color(0,0,255, 20));
          }
      }
  CL_Display::pop_translate_offset();
}

/* EOF */
