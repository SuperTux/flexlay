//  $Id: diamond_map.cxx,v 1.6 2003/09/26 14:29:35 grumbel Exp $
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

#include "globals.hxx"
#include "player.hxx"
#include "diamond_map.hxx"
#include "view.hxx"

DiamondMap::DiamondMap(Field<int>* d)
  : dmap(*d),
    sprite("diamond",   resources)
{
  num_diamonds = 0;
  for(int y = 0; y < dmap.get_height(); ++y)
    for(int x = 0; x < dmap.get_width(); ++x)
      {
        if (dmap(x, y))
          num_diamonds += 1;
      }
}

DiamondMap::~DiamondMap()
{
}

void
DiamondMap::draw ()
{
  CL_Rect rect = View::current()->get_clip_rect();

  int start_x = std::max(0, rect.left/64);
  int start_y = std::max(0, rect.top/64);
  int end_x   = std::min(dmap.get_width(), rect.right/64 + 1);
  int end_y   = std::min(dmap.get_height(),    rect.bottom/64 + 1);

  for(int y = start_y;   y < end_y; ++y)
    for(int x = start_x; x < end_x; ++x)
      {
        if (dmap(x, y))
          sprite.draw(x * 64, y * 64);
      }
}

void
DiamondMap::update (float delta)
{
  CL_Vector pos = Player::current()->get_pos();

  dmap(int(pos.x)/64, int(pos.y)/64)   = 0;
  dmap(int(pos.x)/64, int(pos.y)/64-1) = 0;
  dmap(int(pos.x)/64, int(pos.y)/64-2) = 0;

  sprite.update(delta);
}

int 
DiamondMap::get_num_diamonds()
{
  return num_diamonds;
}

/* EOF */
