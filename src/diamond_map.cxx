//  $Id: diamond_map.cxx,v 1.1 2003/09/12 16:31:20 grumbel Exp $
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
#include "diamond_map.hxx"

DiamondMap::DiamondMap(int w, int h)
  : width(w*2), height(h*2),
    sprite("diamond",   resources)
{
  dmap.resize(width*height);

  for(int y = 0; y < height; ++y)
    for(int x = 0; x < width; ++x)
      dmap[width * y + x] = (rand()%5 == 1);
}

DiamondMap::~DiamondMap()
{
}

void
DiamondMap::draw ()
{
  for(int y = 0; y < height; ++y)
    for(int x = 0; x < width; ++x)
      {
        if (dmap[width * y + x])
          sprite.draw(x * 64, y * 64);
      }
}

void
DiamondMap::update (float delta)
{
  sprite.update(delta);
}

/* EOF */
