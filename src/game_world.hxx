//  $Id: game_world.hxx,v 1.9 2003/09/27 20:57:39 grumbel Exp $
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

#ifndef GAMEWORLD_HXX
#define GAMEWORLD_HXX

#include <list>
#include <vector>
#include <string>

class TileMap;
class DiamondMap;
class WaterMap;
class GameObj;
class Player;

class GameWorld
{
private:
  std::list<GameObj*> objects;
  std::list<Player*> player_objects;
  TileMap* tilemap;
  TileMap* background_tilemap;
  float passed_time;
  DiamondMap* diamond_map;
  WaterMap*   water_map;

  std::vector<std::string> scripts;

  static GameWorld* current_;
public:
  static GameWorld* current() { return current_; }

  GameWorld (int w, int h);
  GameWorld (const std::string& filename);
  ~GameWorld();
  
  void add (GameObj* obj) { objects.push_back (obj); }
  void remove (GameObj* obj) { objects.remove (obj); }
  
  // FIXME: are add/remove for different game object types a good
  // idea?
  void add_player (Player* obj);
  void remove_player (Player* obj);

  void draw ();
  void update (float delta);

  float get_time () { return passed_time; } 

  /** return width in pixels */
  int get_width () const;

  /** return height in pixels */
  int get_height () const;

  void on_startup();

  std::list<Player*>* get_players () { return &player_objects; }

  TileMap* get_tilemap () const { return tilemap; }
  WaterMap* get_watermap () const { return water_map; }
  DiamondMap* get_diamond_map() const { return diamond_map; } 
};

#endif

/* EOF */
