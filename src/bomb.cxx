//  $Id: bomb.cxx,v 1.3 2003/09/28 10:55:34 grumbel Exp $
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
#include "igel.hxx"
#include "game_world.hxx"
#include "bomb.hxx"

Bomb::Bomb(int x, int y)
  : sprite("bomb", resources),
    explo("explo", resources),
    pos(x,
        (y/SUBTILE_SIZE+1)*SUBTILE_SIZE),
    count(1.5f),
    state(COUNTDOWN),
    exploded(false)
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
      if (!exploded)
        {
          exploded = true;
          explode();
        }

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

void 
Bomb::explode()
{
  std::list<GameObj*>* objs = GameWorld::current()->get_objects();
  for(std::list<GameObj*>::iterator i = objs->begin(); i != objs->end(); ++i)
    {
      Igel* igel = dynamic_cast<Igel*>(*i);
      if (igel)
        {
          if (igel->get_pos().x > pos.x - 20 &&
              igel->get_pos().x < pos.x + 20 &&
              igel->get_pos().y > pos.y - 20 &&
              igel->get_pos().y < pos.y + 20)
          igel->die();
        }
    }
}

/* EOF */
