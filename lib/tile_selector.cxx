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
#include <ClanLib/core.h>
#include "math.hxx"
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
  region_select = false;
  offset = 0;
}

TileSelector::~TileSelector()
{
  std::cout << "~TileSelector()" << std::endl;
}

CL_Rect 
TileSelector::get_selection()
{
  CL_Rect selection(current_pos.x, current_pos.y, 
                    region_select_start.x, region_select_start.y);

  selection.normalize();
  selection.right  += 1;
  selection.bottom += 1;

  selection.left  = Math::mid(0, selection.left, width);
  selection.right = Math::mid(0, selection.right, width);

  selection.top    = Math::max(0, selection.top);
  
  return selection;
}

void
TileSelector::mouse_up(const CL_InputEvent& event)
{
  if (event.id == CL_MOUSE_MIDDLE)
    {
      scrolling = false;
      release_mouse();
    }
  else if (event.id == CL_MOUSE_RIGHT)
    {
      release_mouse();
      region_select = false;

      CL_Rect selection = get_selection();
      //selection.bottom = Math::mid(0, selection.right, width);

      TileBrush brush(selection.get_width(), selection.get_height());
      brush.set_transparent();

      for(int y = 0; y < selection.get_height(); ++y)
        for(int x = 0; x < selection.get_width(); ++x)
          {
            int tile = (selection.top + y) * width + (selection.left + x);

            if (tile >= 0 && tile < int(tiles.size()))
              brush.at(x, y) = tiles[tile];
            else
              brush.at(x, y) = 0;
          }

      TileMapPaintTool::current().set_brush(brush);
    }
}

void
TileSelector::mouse_down(const CL_InputEvent& event)
{
  if (event.id == CL_MOUSE_LEFT)
    {
      TileBrush brush(1, 1);

      brush.set_opaque();
      if (mouse_over_tile >= 0 && mouse_over_tile < int(tiles.size()))
        brush.at(0, 0) = tiles[mouse_over_tile];
      else
        brush.at(0, 0) = 0;

      TileMapPaintTool::current().set_brush(brush);
    }
  else if (event.id == CL_MOUSE_RIGHT) 
    {
      region_select = true;
      region_select_start = current_pos;
      capture_mouse();
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

CL_Point
TileSelector::get_mouse_tile_pos(const CL_InputEvent& event)
{
  return CL_Point(event.mouse_pos.x/static_cast<int>(tileset.get_tile_size()*scale),
                  (event.mouse_pos.y+offset)/static_cast<int>(tileset.get_tile_size()*scale));
}

void
TileSelector::mouse_move(const CL_InputEvent& event)
{
  CL_Point pos = get_mouse_tile_pos(event);
  current_pos = pos;
  mouse_over_tile = pos.y * width + pos.x;

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
  CL_Display::push_cliprect(get_screen_rect());
  CL_Display::push_modelview();
  CL_Display::add_translate(get_screen_x(), get_screen_y());
  CL_Display::add_translate(0, -offset);

  const TileBrush& brush = TileMapPaintTool::current().get_brush();

  int start_row = offset / tileset.get_tile_size();
  int end_row   = start_row + (get_screen_rect().get_height() / tileset.get_tile_size());
  int end_index = std::min(end_row*width, int(tiles.size())); 
  
  // Draw tiles
  for(int i = (start_row*width); i < end_index; ++i)
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

      if (brush.get_width() == 1 && brush.get_height() == 1
          && brush.at(0, 0) == tiles[i])
        {
          CL_Display::fill_rect(rect,
                                CL_Color(0,0,255, 100));
        }
      else if (mouse_over_tile == int(i) && has_mouse_over())
        {
          CL_Display::fill_rect(rect, CL_Color(0,0,255, 20));
        }
    }

  if (region_select)
    {
      CL_Rect rect = get_selection();

      rect.top    *= static_cast<int>(tileset.get_tile_size()*scale);
      rect.bottom *= static_cast<int>(tileset.get_tile_size()*scale);
      rect.left   *= static_cast<int>(tileset.get_tile_size()*scale);
      rect.right  *= static_cast<int>(tileset.get_tile_size()*scale);

      CL_Display::fill_rect(rect, CL_Color(0,0,255, 100));
    }
  
  CL_Display::pop_modelview();
  CL_Display::pop_cliprect();
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
  offset = 0;
}

/* EOF */
