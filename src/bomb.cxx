//  $Id: bomb.cxx,v 1.1 2003/09/20 21:55:57 grumbel Exp $
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
#include "bomb.hxx"

Bomb::Bomb(int x, int y)
  : sprite("bomb", resources),
    explo("explo", resources),
    pos(x,
        (y/SUBTILE_SIZE+1)*SUBTILE_SIZE),
    count(3.0f),
    state(COUNTDOWN)
{
}

Bomb::~Bomb()
{
}

void
Bomb::update(float delta)
{
  if (explo.is_finished())
    remove();

  if (state == EXPLODE)
    explo.update(delta);
  else
    sprite.update(delta);

  count -= delta;

  if (count < 0 && state != EXPLODE)
    {
      state = EXPLODE;
      count = 0;
    }
}

void
Bomb::draw()
{
  if (state == EXPLODE)
    explo.draw(pos.x, pos.y);
  else
    sprite.draw(pos.x, pos.y);
}

/* EOF */
