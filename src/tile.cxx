//  $Id: tile.cxx,v 1.3 2003/08/12 08:24:41 grumbel Exp $
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

#include "tile.hxx"

Tile::Tile(CL_Sprite arg_sur, unsigned char arg_colmap[])
  : sur(arg_sur)
{
  //sur.set_alignment(origin_center, 0, 0);
  memcpy(colmap, arg_colmap, 8);
}

/* EOF */
