//  $Id: PowerUp.hxx,v 1.1 2002/03/19 17:56:53 grumbel Exp $
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

#ifndef POWERUP_HXX
#define POWERUP_HXX

#include <ClanLib/core.h>
#include <ClanLib/display.h>
#include <SphriteLib/sphritelib.h>
#include "GameObj.hxx"
#include "Player.hxx"
#include "globals.hxx"

class PowerUp : public GameObj
{
private:
  Sprite* sprite;
  CL_Vector pos;
public:
  PowerUp (Sprite* s, const CL_Vector&);

  void draw ();
  void update (float delta);
protected:
  // Called
  virtual void player_catched (Player*) =0;
};

class ShildPowerUp : public PowerUp 
{
public:
  ShildPowerUp (const CL_Vector& pos);
  void player_catched (Player*);
};

class SpreadPowerUp : public PowerUp 
{
public:
  SpreadPowerUp (const CL_Vector& pos);
  void player_catched (Player*);
};


#endif

/* EOF */
