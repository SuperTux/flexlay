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

#include <ClanLib/Display/display.h>
#include <iostream>
#include <ClanLib/Core/core_iostream.h>
#include "math.hxx"
#include "tilemap_layer.hxx"
#include "tileset.hxx"
#include "tile_selection.hxx"

class TileSelectionImpl
{
public:
  TilemapLayer tilemap;
  CL_Point start_pos;
  CL_Rect  selection;
  bool active;
};

TileSelection::TileSelection()
  : impl(new TileSelectionImpl())
{
  impl->active = false;
}

TileSelection::~TileSelection()
{
}

void
TileSelection::start(TilemapLayer tilemap_, const CL_Point& pos)
{
  impl->tilemap = tilemap_;
  impl->active = true;
  impl->start_pos = pos;
  update(impl->start_pos);
}

void
TileSelection::update(const CL_Point& pos)
{
  impl->selection = CL_Rect(std::min(impl->start_pos.x, pos.x),
                            std::min(impl->start_pos.y, pos.y),
                            std::max(impl->start_pos.x, pos.x) + 1,
                            std::max(impl->start_pos.y, pos.y) + 1);
}

bool
TileSelection::is_active()
{
  return impl->active;
}

void
TileSelection::clear()
{
  impl->selection = CL_Rect();
  impl->active = false;
}

void
TileSelection::draw(const CL_Color& color)
{
  int tile_size = impl->tilemap.get_tileset().get_tile_size();

  CL_Display::fill_rect(CL_Rect(impl->selection.left   * tile_size, 
                                impl->selection.top    * tile_size,
                                impl->selection.right  * tile_size, 
                                impl->selection.bottom * tile_size),
                        color);
}

TileBrush
TileSelection::get_brush(const Field<int>& field) const
{
  CL_Rect sel = impl->selection;

  sel.normalize();

  if (sel.left     > field.get_width() - 1
      || sel.top   > field.get_height() - 1
      || sel.right  <= 0
      || sel.bottom <= 0)
    { // Selection is empty
      std::cout << "Error: Invalid selection" << std::endl;
      TileBrush brush(1, 1);
      brush.at(0, 0) = 0;
      brush.set_opaque();
      return brush;
    }
  else
    { // Selection is valid
      // Cut the selection to the field size
      sel.left = Math::max(0, sel.left);
      sel.top  = Math::max(0, sel.top);

      sel.right  = Math::min(sel.right,  field.get_width()); 
      sel.bottom = Math::min(sel.bottom, field.get_height()); 

      TileBrush brush(sel.get_width(), 
                      sel.get_height());

      for(int y = sel.top; y < sel.bottom; ++y)
        for(int x = sel.left; x < sel.right; ++x)
          {
            brush.at(x - sel.left, 
                     y - sel.top) = field.at(x, y);
          }

      return brush;
    }
}

CL_Rect
TileSelection::get_rect() const
{
  CL_Rect sel = impl->selection;
  sel.normalize();
  return sel;
}

/* EOF */
