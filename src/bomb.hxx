//  $Id: bomb.hxx,v 1.2 2003/09/28 10:55:34 grumbel Exp $
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

#ifndef HEADER_BOMB_HXX
#define HEADER_BOMB_HXX

#include <ClanLib/Display/sprite.h>
#include <ClanLib/Core/Math/point.h>
#include "gameobj.hxx"

/** */
class Bomb : public GameObj
{
private:
  CL_Sprite sprite;
  CL_Sprite explo;
  CL_Point pos;
  float count;
  enum { COUNTDOWN, EXPLODE } state;
  bool exploded;

public:
  Bomb(int x, int y);
  virtual ~Bomb();

  void update(float delta);
  void draw();
private:
  void explode();
  Bomb (const Bomb&);
  Bomb& operator= (const Bomb&);
};

#endif

/* EOF */
