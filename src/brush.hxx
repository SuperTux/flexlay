//  $Id: brush.hxx,v 1.1 2003/11/05 22:51:27 grumbel Exp $
// 
//  Windstille - A Jump'n Shoot Game
//  Copyright (C) 2003 Ingo Ruhnke <grumbel@gmx.de>
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

#ifndef HEADER_BRUSH_HXX
#define HEADER_BRUSH_HXX

#include <ClanLib/Display/sprite.h>
#include "gameobj.hxx"

/** */
class Brush : public GameObj
{
private:
  CL_Sprite sprite;
  CL_Point pos;
  bool blink;
  float passed_time;

public:
  Brush(const std::string& res, CL_Point pos, bool blink);
  virtual ~Brush();

  void draw();
  void update(float delta);

private:
  Brush (const Brush&);
  Brush& operator= (const Brush&);
};

#endif

/* EOF */
