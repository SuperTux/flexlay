//  $Id: game.cxx,v 1.7 2003/09/21 15:22:59 grumbel Exp $
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
#include "trigger.hxx"
#include "dialog_manager.hxx"
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

void add_region_trigger(int x, int y, int w, int h, SCM func)
{
  GameWorld::current()->add(new Trigger(new RegionTriggerCondition(CL_Rectf(x, y, w, h)), 
                                        func));
}

void player_set_pos(float x, float y)
{
  Player::current()->set_position(CL_Vector(x, y));
}

void player_set_direction(const char* direction)
{
  if (strcmp(direction, "east") == 0)
    {
      Player::current()->set_direction(EAST);
    }
  else if (strcmp(direction, "west") == 0)
    {
      Player::current()->set_direction(WEST);
    }
  else
    {
      std::cout << __FUNCTION__ << ": Unknown direction:" << direction << std::endl;
    }
}

int mouse_get_x() 
{
  return CL_Mouse::get_x();
}

int mouse_get_y() 
{
  return CL_Mouse::get_y();
}


int screen_get_x() 
{
  return CL_Mouse::get_x();
}

int screen_get_y() 
{
  return CL_Mouse::get_y();
}

int player_get_x()
{
  return int(Player::current()->get_pos().x);
}

int player_get_y()
{
  return int(Player::current()->get_pos().y);
}

void game_set_pause(bool p)
{
  WindstilleGame::current()->set_pause(p);
}

bool game_get_pause()
{
  return WindstilleGame::current()->get_pause();
}

void game_quit()
{
  WindstilleGame::current()->quit();
}

void dialog_add(const char* portrait, const char* text)
{
  DialogManager::current()->add_dialog(portrait, text);
}

void dialog_show()
{
  WindstilleGame::current()->set_dialog_state();
}

void dialog_hide()
{
  WindstilleGame::current()->set_game_state();
}

void dialog_clear()
{
  DialogManager::current()->clear();
}

/* EOF */
