//  $Id: game_world.cxx,v 1.4 2003/08/12 08:24:41 grumbel Exp $
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

#include "player.hxx"
#include "gameobj.hxx"
#include "game_world.hxx"
#include "windstille_level.hxx"

bool removable (GameObj* obj) {
  return obj->is_removable ();
}

GameWorld::GameWorld (const std::string& filename)
  : passed_time (0)
{
  WindstilleLevel level (filename);
  std::cout << "GameWorld: Creating Tilemap" << std::endl;
  tilemap = new TileMap(&level);
}

void
GameWorld::draw ()
{
  for (std::list<GameObj*>::iterator i = objects.begin ();
       i != objects.end (); ++i)
    (*i)->draw ();

  tilemap->draw ();
}

void
GameWorld::update (float delta)
{
  passed_time += delta;

  std::list<GameObj*> tmp_objects (objects);

  for (std::list<GameObj*>::iterator i = tmp_objects.begin ();
       i != tmp_objects.end (); ++i)
    (*i)->update (delta);
  
  objects.remove_if (removable);
}

int  
GameWorld::get_width () const
{
  return tilemap->get_width () * TILE_SIZE;
}

int 
GameWorld::get_height () const
{
  return tilemap->get_height () * TILE_SIZE;
}

void
GameWorld::add_player (Player* obj)
{ 
  objects.push_back (obj); 
  player_objects.push_back (obj); 
}

void
GameWorld::remove_player (Player* obj)
{ 
  objects.remove (obj); 
  player_objects.remove (obj); 
}

/* EOF */
