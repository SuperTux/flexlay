//  $Id: tile_selector.cxx,v 1.7 2003/09/23 19:10:05 grumbel Exp $
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
#include <ClanLib/display.h>
#include "tileset.hxx"
#include "tile.hxx"
#include "tile_selector.hxx"
#include "tilemap_paint_tool.hxx"

TileSelector::TileSelector(const CL_Rect& rect, CL_Component* parent)
  : CL_Component(rect, parent),
    width(1)
{
  index = 0;

  slots.connect(sig_paint(),      this, &TileSelector::draw);
  slots.connect(sig_mouse_move(), this, &TileSelector::mouse_move);
  slots.connect(sig_mouse_down(), this, &TileSelector::mouse_down);
  slots.connect(sig_mouse_up  (), this, &TileSelector::mouse_up);
 
  scale = 1.0f;
  mouse_over_tile = -1;
  scrolling = false;
  offset = 0;
}

TileSelector::~TileSelector()
{
  std::cout << "~TileSelector()" << std::endl;
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
      TileBrush brush(1, 1);

      brush.set_opaque();
      brush.at(0, 0) = mouse_over_tile;

      TileMapPaintTool::current().set_brush(brush);
    }
  else if (event.id == CL_MOUSE_MIDDLE)
    {
      scrolling = true;
      mouse_pos = event.mouse_pos;
      old_offset = offset;
      capture_mouse();
    }
  else if (event.id == CL_MOUSE_WHEEL_UP)
    {
      offset -= static_cast<int>(tileset.get_tile_size()*scale);
      if (offset < 0)
        offset = 0;
    }
  else if (event.id == CL_MOUSE_WHEEL_DOWN)
    {
      offset += static_cast<int>(tileset.get_tile_size()*scale);
    }
}

void
TileSelector::mouse_move(const CL_InputEvent& event)
{
  int x = event.mouse_pos.x/static_cast<int>(tileset.get_tile_size()*scale);
  int y = (event.mouse_pos.y+offset)/static_cast<int>(tileset.get_tile_size()*scale);

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
  CL_Display::push_modelview();
  CL_Display::add_translate(0, -offset);

  for(int i = 0; i < int(tiles.size()); ++i)
    {
      int x = i % width;
      int y = i / width;

      Tile* tile = tileset.create(tiles[i]);

      CL_Rect rect(CL_Point(static_cast<int>(x * tileset.get_tile_size()*scale),
                            static_cast<int>(y * tileset.get_tile_size()*scale)),
                   CL_Size(static_cast<int>(tileset.get_tile_size()*scale),
                           static_cast<int>(tileset.get_tile_size()*scale)));

      if (tile)
        {
          CL_Sprite sprite = tile->get_sprite();

          sprite.set_scale(scale, scale);

          sprite.draw(static_cast<int>(x * tileset.get_tile_size()*scale), 
                      static_cast<int>(y * tileset.get_tile_size()*scale));

          CL_Display::draw_rect(rect, CL_Color(0,0,0,128));
        }

      if (TileMapPaintTool::current().get_brush().get_width() == 1
          && TileMapPaintTool::current().get_brush().get_height() == 1
          && TileMapPaintTool::current().get_brush().at(0, 0) == i)
        {
          CL_Display::fill_rect(rect,
                                CL_Color(0,0,255, 100));
        }
      else if (mouse_over_tile == int(i) && has_mouse_over())
        {
          CL_Display::fill_rect(rect, CL_Color(0,0,255, 20));
        }
    }
  
  CL_Display::pop_modelview();
}

void
TileSelector::set_scale(float s)
{
  scale = s;
}

TileSelector::Tiles
TileSelector::get_tiles() const
{
  return tiles;
}

void
TileSelector::set_tileset(Tileset t)
{
  tileset = t;
  // Recalc the number of tiles in a row
  width  = get_width()/tileset.get_tile_size();
}

void
TileSelector::set_tiles(const Tiles& t)
{
  tiles = t;
}

/* EOF */
