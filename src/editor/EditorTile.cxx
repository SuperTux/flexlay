//  $Id: EditorTile.cxx,v 1.2 2002/09/01 00:05:33 grumbel Exp $
//
//  Pingus - A free Lemmings clone
//  Copyright (C) 2000 Ingo Ruhnke <grumbel@gmx.de>
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

#include <string>
#include "../globals.hxx"
#include "EditorTile.hxx"

EditorTile::EditorTile ()
{
  set_tile ("none");
}

EditorTile::EditorTile (const std::string& str)
{
  set_tile (str);
}

void
EditorTile::draw (int x, int y)
{
  //std::cout << "drawing: " << x << "x" << y << std::endl;
  if (sprite)
    {
      sprite->set_alignment (origin_top_left, 0, 0);
      sprite->draw (x, y);
      //CL_Display::draw_rect (x, y, x + 64, y + 64, 1.0, 1.0, 1.0, 1.0);
    }
  else
    {
      CL_Display::fill_rect (CL_Rect(x, y, x + 64, y + 64),
			     CL_Color(77, 77, 77, 255));
      CL_Display::draw_rect (CL_Rect(x, y, x + 64, y + 64),
			     CL_Color(255, 255, 255, 255));
    }
}

void
EditorTile::set_tile (const std::string& str)
{
  //std::cout << "str: " << str << std::endl;
  if (str == "none")
    {
      sprite = 0;
      name = str;
    }
  else
    {
      assert (false);
#if 0
      sprite = sprite_storage->create (str.c_str ());
#endif
      name = str;
    }
}

/* EOF */
