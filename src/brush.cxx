//  $Id: brush.cxx,v 1.1 2003/11/05 22:51:27 grumbel Exp $
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

#include "globals.hxx"
#include "brush.hxx"

Brush::Brush(const std::string& res, CL_Point pos, bool blink)
  : sprite(res, resources),
    pos(pos),
    blink(blink),
    passed_time(0)
{
  if (blink)
    sprite.set_blend_func(blend_src_alpha, blend_one);
}

Brush::~Brush()
{
}

void
Brush::draw()
{
  sprite.draw(pos.x, pos.y);
}

void
Brush::update(float delta)
{
  passed_time += delta;

  if (blink)
    {
      sprite.set_alpha(cos(passed_time*3.141f)*.2f + .8f);
    }
}

/* EOF */
