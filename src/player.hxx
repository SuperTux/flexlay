//  $Id: player.hxx,v 1.6 2003/09/12 20:17:06 grumbel Exp $
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

#ifndef PLAYER_HXX
#define PLAYER_HXX

#include <ClanLib/core.h>
#include <ClanLib/gl.h>
#include <ClanLib/display.h>

#include "globals.hxx"
#include "gameobj.hxx"

class Controller;

/** A position in units of subtiles */
struct SubTilePos {
  SubTilePos() : x(0), y(0) {}

  SubTilePos(int x_, int y_) 
    : x(x_), y(y_)
  {}

  bool operator==(const SubTilePos& pos) {
    return pos.x == x && pos.y == y;
  }

  int x;
  int y;
};

class Player : public GameObj
{
private:
  Controller* controller;

  /** Position as a float */
  CL_Vector pos;
  
  SubTilePos subtile_pos;

  /** X-Position in subtile coordinates */
  int x_pos;
  /** Y-Position in subtile coordinates */
  int y_pos;
  
  CL_Vector velocity;
  
  CL_Sprite walk;
  CL_Sprite jump;
  CL_Sprite stand;

  CL_Sprite sit;
  CL_Sprite roll;
  CL_Sprite surround;
  
public:
  typedef enum { WALKING, SITTING, STANDING } MovementState;
  typedef enum { GUN_READY, GUN_RELOADING } GunState;
  typedef enum { ON_GROUND, IN_AIR } GroundState;

  //{ BOUNCE, SPREAD, LASER }
private:
  MovementState state;
  GunState gun_state;
  Direction direction;
  GroundState ground_state;
  
  double reload_time;
  static Player* current_;
public:
  Player (Controller*);
  virtual ~Player () {}

  static Player* current() { return current_; }

  void draw ();
  void update (float delta);

  void set_position (const CL_Vector& arg_pos);
  void set_direction (Direction dir);

  CL_Vector get_pos () const { return pos; }
  SubTilePos get_subtile_pos();

private:
  // true if the tile under Player is ground
  bool on_ground ();
  // true if Player is inside a ground tile
  bool stuck ();

  void update_ground (float delta);
  void update_air (float delta);
  void update_shooting (float delta);
};

#endif

/* EOF */
