//  $Id: igel.cxx,v 1.2 2003/09/27 20:57:39 grumbel Exp $
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
#include "game_world.hxx"
#include "tile_map.hxx"
#include "igel.hxx"

Igel::Igel(int x, int y)
  : sprite("igel", resources),
    pos(x, y)
{
  direction_left = false;
}

Igel::~Igel()
{
}

void
Igel::draw()
{
  if (direction_left)
    sprite.set_scale(1.0f, 1.0f);
  else
    sprite.set_scale(-1.0f, 1.0f);
  sprite.draw(int(pos.x), int(pos.y));
}

void
Igel::update(float delta)
{
  sprite.update(delta);
  
  CL_Pointf old_pos = pos;

  if (on_ground())
    {
      pos.y = int(pos.y)/SUBTILE_SIZE * SUBTILE_SIZE;

      if (direction_left)
        pos.x -= 32 * delta;
      else
        pos.x += 32 * delta;      

      if (!on_ground() || in_wall())
        {
          direction_left = !direction_left;
          pos = old_pos;
        }
    }
  else
    { // Fall
      pos.y += 450 * delta;
    }

  // Check if the player got hit
  // FIXME: Insert pixel perfect collision detection here
  CL_Vector player_pos = Player::current()->get_pos();
  if (pos.x - 20 < player_pos.x
      && pos.x + 20 > player_pos.x
      && pos.y - 20 < player_pos.y
      && pos.y + 5  > player_pos.y)
    Player::current()->hit(3);
}

bool
Igel::in_wall()
{
  return GameWorld::current()->get_tilemap()->is_ground((int(pos.x)/SUBTILE_SIZE)*SUBTILE_SIZE,
                                                        (int(pos.y)/SUBTILE_SIZE - 1)*SUBTILE_SIZE)
    || GameWorld::current()->get_tilemap()->is_ground((int(pos.x)/SUBTILE_SIZE - 1)*SUBTILE_SIZE,
                                                        (int(pos.y)/SUBTILE_SIZE - 1)*SUBTILE_SIZE)
    || GameWorld::current()->get_tilemap()->is_ground((int(pos.x)/SUBTILE_SIZE - 2)*SUBTILE_SIZE,
                                                        (int(pos.y)/SUBTILE_SIZE - 1)*SUBTILE_SIZE)
    || GameWorld::current()->get_tilemap()->is_ground((int(pos.x)/SUBTILE_SIZE + 1)*SUBTILE_SIZE,
                                                        (int(pos.y)/SUBTILE_SIZE - 1)*SUBTILE_SIZE)
    || GameWorld::current()->get_tilemap()->is_ground((int(pos.x)/SUBTILE_SIZE + 2)*SUBTILE_SIZE,
                                                      (int(pos.y)/SUBTILE_SIZE - 1)*SUBTILE_SIZE);
}

bool
Igel::on_ground()
{
  return GameWorld::current()->get_tilemap()->is_ground((int(pos.x)/SUBTILE_SIZE)*SUBTILE_SIZE,
                                                        (int(pos.y)/SUBTILE_SIZE)*SUBTILE_SIZE)
    && GameWorld::current()->get_tilemap()->is_ground((int(pos.x)/SUBTILE_SIZE+1)*SUBTILE_SIZE,
                                                      (int(pos.y)/SUBTILE_SIZE)*SUBTILE_SIZE)
    && GameWorld::current()->get_tilemap()->is_ground((int(pos.x)/SUBTILE_SIZE+2)*SUBTILE_SIZE,
                                                      (int(pos.y)/SUBTILE_SIZE)*SUBTILE_SIZE)
    && GameWorld::current()->get_tilemap()->is_ground((int(pos.x)/SUBTILE_SIZE - 1)*SUBTILE_SIZE,
                                                      (int(pos.y)/SUBTILE_SIZE)*SUBTILE_SIZE)
    && GameWorld::current()->get_tilemap()->is_ground((int(pos.x)/SUBTILE_SIZE - 2)*SUBTILE_SIZE,
                                                      (int(pos.y)/SUBTILE_SIZE)*SUBTILE_SIZE);
}

/* EOF */
