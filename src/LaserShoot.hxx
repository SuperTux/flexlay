//  $Id: LaserShoot.hxx,v 1.1 2002/03/19 17:56:57 grumbel Exp $
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

#ifndef LASERSHOOT_HXX
#define LASERSHOOT_HXX

#include <SphriteLib/sphritelib.h>
#include <ClanLib/core.h>
#include "globals.hxx"
#include "GameObj.hxx"

class LaserShoot : public GameObj
{
private:
  CL_Vector pos;
  Direction direction;
  int stage;
  Sprite* sprite;
public:
  LaserShoot (const CL_Vector& arg_pos, Direction arg_dir, int arg_stage);

  void draw ();
  void update (float delta);
};

#endif

/* EOF */
