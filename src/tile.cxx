//  $Id: tile.cxx,v 1.4 2003/09/22 18:37:05 grumbel Exp $
//
//  Windstille - A Jump'n Shoot Game
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

#include <ClanLib/Core/System/error.h>
#include <iostream>
#include "globals.hxx"
#include "tile.hxx"

Tile::Tile(const std::string& filename_, 
           const CL_Color& color_, 
           const CL_Color& attribute_color_, 
           unsigned char arg_colmap[])
  : color(color_),
    attribute_color(attribute_color_),
    filename(filename_)
{
  //sur.set_alignment(origin_center, 0, 0);
  memcpy(colmap, arg_colmap, 8);
}

CL_Color
Tile::get_color()
{
  return color;
}

CL_Color
Tile::get_attribute_color()
{
  return attribute_color;
}

CL_Sprite&
Tile::get_sprite()
{
  if (sur)
    return sur;
  else
    {
      try {
        //std::cout << "Loading Tile: " << filename << std::endl;
        sur = CL_Sprite(filename, resources);
        return sur;
      } catch (CL_Error& err) {
        std::cout << "Tile: CL_Error: " << err.message << std::endl;
        assert(0);
      }
    }
}

/* EOF */
