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
#include "tilemap.hxx"
#include "tileset.hxx"
#include "tile_selection.hxx"

TileSelection::TileSelection()
{
  active = false;
}

TileSelection::~TileSelection()
{
}

void
TileSelection::start(TileMap* tilemap_, const CL_Point& pos)
{
  tilemap = tilemap_;
  active = true;
  start_pos = pos;
  update(start_pos);
}

void
TileSelection::update(const CL_Point& pos)
{
  selection = CL_Rect(std::min(start_pos.x, pos.x),
                      std::min(start_pos.y, pos.y),
                      std::max(start_pos.x, pos.x) + 1,
                      std::max(start_pos.y, pos.y) + 1);
}

bool
TileSelection::is_active()
{
  return active;
}

void
TileSelection::clear()
{
  selection = CL_Rect();
  active = false;
}

void
TileSelection::draw(const CL_Color& color)
{
  int tile_size = tilemap->get_tileset()->get_tile_size();

  CL_Display::fill_rect(CL_Rect(selection.left  * tile_size, selection.top    * tile_size,
                                selection.right * tile_size, selection.bottom * tile_size),
                        color);
}

TileBrush
TileSelection::get_brush(const Field<int>& field) const
{
  CL_Rect sel = selection;

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

/* EOF */
