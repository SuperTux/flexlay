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

#include <ClanLib/Core/Math/rect.h>
#include <ClanLib/Display/display.h>
#include "editor_grid_layer.hxx"
#include "editor_map_component.hxx"

EditorGridLayer::EditorGridLayer(const CL_Point pos_, int w, int h, int tile_size_)
  : pos(pos_),
    width(w),
    height(h),
    tile_size(tile_size_)
{
}

void
EditorGridLayer::draw(EditorMapComponent* parent)
{
  CL_Rect rect = parent->get_clip_rect();

  int start_x = std::max(0, rect.left/tile_size);
  int start_y = std::max(0, rect.top/tile_size);
  int end_x   = std::min(width,  rect.right/tile_size + 1);
  int end_y   = std::min(height, rect.bottom/tile_size + 1);

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
