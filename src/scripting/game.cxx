//  $Id: game.cxx,v 1.4 2003/09/15 17:00:38 grumbel Exp $
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

#include "scripting/game.hxx"
#include "water_map.hxx"
#include "game_world.hxx"
#include "igel.hxx"
#include "bomb.hxx"
#include "windstille_game.hxx"
#include "player.hxx"

void
game_add_water(int x, int y, int w, int h)
{
  GameWorld::current()->get_watermap()->add_water(x, y, w, h);
}

void
game_add_bomb(int x, int y)
{
  GameWorld::current()->add(new Bomb(x, y));
}

void
game_add_igel(int x, int y)
{
  GameWorld::current()->add(new Igel(x, y));
}

void
game_set_player(float x, float y)
{
  Player::current()->set_position(CL_Vector(x, y));
}

GameWorld* make_game_world(int w, int h)
{
  return new GameWorld(w, h);
}

void start_game(GameWorld* world)
{
  WindstilleGame game (world);
  game.display ();
}

/* EOF */
