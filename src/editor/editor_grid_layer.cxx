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

#include "editor_grid_layer.hxx"

EditorGridLayer::EditorGridLayer(const CL_Point pos_, int w, int h)
  : pos(pos_),
    width(w),
    height(h)
{
}

void
EditorGridLayer::draw(EditorMapComponent* parent)
{
  for (int y = start_y; y <= end_y; ++y)
    CL_Display::draw_line(start_x * tile_size,
                          y * tile_size,
                          end_x   * tile_size,
                          y * tile_size, 
                          CL_Color(150, 150, 150));
  
  for (int x = start_x; x <= end_x; ++x)
    CL_Display::draw_line(x * tile_size,
                          start_y * tile_size,
                          x   * tile_size,
                          end_y * tile_size, 
                          CL_Color(150, 150, 150));
}

/* EOF */
