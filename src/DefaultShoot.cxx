//  $Id: DefaultShoot.cxx,v 1.1 2002/03/19 17:56:51 grumbel Exp $
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

#include "GameWorld.hxx"
#include "AnimationObj.hxx"
#include "DefaultShoot.hxx"

extern CL_ResourceManager* resources;
extern SpriteProviderStorage* sprite_storage;

DefaultShoot::DefaultShoot (const CL_Vector& arg_pos,
			    DefaultShoot::DirectionState dir)
  : pos (arg_pos),
    sprite (sprite_storage->create("shoot/default")),
    direction (dir)
{ 
}

void
DefaultShoot::draw ()
{
  if (direction)
    sprite->setScale (1.0, 1.0);
  else
    sprite->setScale (-1.0, 1.0);

  sprite->draw ((int)pos.x, (int) pos.y);
}

void
DefaultShoot::update (float delta)
{
  switch (direction)
    {
    case WEST:
      pos.x -= 800 * delta;
      break;
    case EAST:
      pos.x += 800 * delta;
      break;
    }

  if (get_world ()->get_tilemap()->is_ground (pos.x, pos.y))
    explode ();
}

void
DefaultShoot::explode ()
{
  remove ();
  get_world ()->add (new AnimationObj ("shoot/explosion", pos));
}

/* EOF */
