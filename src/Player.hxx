//  $Id: Player.hxx,v 1.2 2002/09/01 00:05:33 grumbel Exp $
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

#ifndef PLAYER_HXX
#define PLAYER_HXX

#include <ClanLib/core.h>
#include <ClanLib/display.h>

#include "globals.hxx"
#include "GameObj.hxx"

class Controller;

class Player : public GameObj
{
private:
  Controller* controller;
  CL_Vector pos;
  CL_Vector velocity;
  
  CL_Sprite walk;
  CL_Sprite jump;
  CL_Sprite stand;
  CL_Sprite shild;

  CL_Sprite sit;
  CL_Sprite roll;
  CL_Sprite surround;
  
public:
  typedef enum { SURROUND, WALKING, SITTING, STANDING, ROLLING } MovementState;
  typedef enum { GUN_READY, GUN_RELOADING } GunState;
  typedef enum { ON_GROUND, IN_AIR } GroundState;

  //{ BOUNCE, SPREAD, LASER }
private:
  MovementState state;
  GunState gun_state;
  Direction direction;
  GroundState ground_state;
  
  double reload_time;
  float shild_time;
public:
  Player (Controller*);
  virtual ~Player () {}

  void draw ();
  void update (float delta);

  void set_position (const CL_Vector& arg_pos);
  void set_direction (Direction dir);

  CL_Vector get_pos () const { return pos; }

  void activate_shild ();

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
