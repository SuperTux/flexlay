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

#include <ClanLib/Core/Math/rect.h>
#include <iostream>
#include <ClanLib/Core/core_iostream.h>
#include "tile_brush.hxx"

TileBrush::TileBrush()
{
  opaque = false;
}

TileBrush::TileBrush(int w, int h)
  : Field<int>(w, h)
{
}

void
TileBrush::auto_crop()
{
  CL_Rect rect(CL_Point(0, 0), CL_Size(0, 0));

  for(int y = 0; y < get_height(); ++y)
    for(int x = 0; x < get_width(); ++x)
      if (at(x, y) != 0)
        {
          rect.top = y;
          goto bottom;
        }

 bottom:
  for(int y = get_height()-1; y >= 0; --y)
    for(int x = 0; x < get_width(); ++x)
      if (at(x, y) != 0)
        {
          rect.bottom = y + 1;
          goto left;
        }

 left:
  for(int x = 0; x < get_width(); ++x)
    for(int y = 0; y < get_height(); ++y)
      if (at(x, y) != 0)
        {
          rect.left = x;
          goto right;
        }

 right:
  for(int x = get_width() - 1; x >= 0; --x)
    for(int y = 0; y < get_height(); ++y)
      if (at(x, y) != 0)
        {
          rect.right = x + 1;
          goto end;
        }

 end:
  resize(rect.get_width(), rect.get_height(), 
         -rect.left, -rect.top);
}

/* EOF */
