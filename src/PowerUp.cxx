//  $Id: PowerUp.cxx,v 1.1 2002/03/19 17:56:53 grumbel Exp $
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
#include "PowerUp.hxx"

PowerUp::PowerUp (Sprite* s, const CL_Vector& arg_pos)
  : sprite (s), pos (arg_pos)
{
}

void
PowerUp::draw ()
{
  sprite->draw (int (pos.x), int (pos.y));
}

void
PowerUp::update (float delta)
{
  pos.y -= 10*delta;
  
  std::list<Player*>* lst = get_world ()->get_players ();

  for (std::list<Player*>::iterator i = lst->begin (); i != lst->end (); ++i)
    {
      //std::cout << "Abstand: " << fabs((pos - (*i)->get_pos ()).norm ()) << std::endl;
      // FIXME: Replace this with Real Collision Detection[tm]
      if (fabs((pos - (*i)->get_pos ()).norm ()) < 20.0f)
	{
	  player_catched (*i);
	  remove ();
	}
    }
}

ShildPowerUp::ShildPowerUp (const CL_Vector& pos)
  : PowerUp (sprite_storage->create ("powerup/shild"), pos)
{}

SpreadPowerUp::SpreadPowerUp (const CL_Vector& pos)
  : PowerUp (sprite_storage->create ("powerup/spread"), pos)
{}

void
ShildPowerUp::player_catched (Player* player)
{
  player->activate_shild ();
}

void
SpreadPowerUp::player_catched (Player*)
{
}

/* EOF */
