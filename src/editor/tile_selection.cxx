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

#include "tile_selection.hxx"

TileSelection::TileSelection()
{
  active = false;
}

TileSelection::~TileSelection()
{
}

void
TileSelection::start(const CL_Point& pos)
{
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
TileSelection::draw()
{
}

TileBrush
TileSelection::get_brush(const Field<int>& field) const
{
  TileBrush brush(selection.get_width(), 
                  selection.get_height());

  for(int y = selection.top; y < selection.bottom; ++y)
    for(int x = selection.left; x < selection.right; ++x)
      {
        brush.at(x - selection.left, 
                 y - selection.top) = field.at(x, y);
      }

  return brush;
}

/* EOF */
