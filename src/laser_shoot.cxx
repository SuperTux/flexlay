//  $Id: laser_shoot.cxx,v 1.3 2003/08/19 13:40:48 grumbel Exp $
//
//  Windstille - A Jump'n Shoot Game
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

#include <assert.h>
#include "globals.hxx"
#include "string_converter.hxx"
#include "game_world.hxx"
#include "animation_obj.hxx"
#include "laser_shoot.hxx"

LaserShoot::LaserShoot (const CL_Vector& arg_pos, Direction arg_dir, int arg_stage)
  : pos (arg_pos), direction (arg_dir), stage (arg_stage)
{
  assert(stage >= 1 && stage <= 5);

  sprite = CL_Sprite (std::string("shoot/laser/stage") + to_string(stage), resources);
}

void
LaserShoot::draw ()
{
  if (direction == WEST)
    sprite.set_scale (-1.0, 1.0);
  sprite.draw (int (pos.x), int (pos.y));
}

void 
LaserShoot::update (float delta)
{
  sprite.update (delta);
  if (direction == WEST)
    pos.x -= 2000 * delta;
  else
    pos.x += 2000 * delta;

  if (get_world ()->get_tilemap()->is_ground (pos.x, pos.y))
    { 
      get_world ()->add (new AnimationObj ("shoot/explosion", pos));
      remove ();
    }
}

/* EOF */
