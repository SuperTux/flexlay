//  $Id: water_splash.cxx,v 1.1 2003/09/12 20:17:06 grumbel Exp $
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

#include "globals.hxx"
#include "water_splash.hxx"

WaterSplash::WaterSplash(float x, float y)
  : sprite("watersplash", resources)
{
  pos = CL_Vector(x, y);
  time = 0;
}

WaterSplash::~WaterSplash()
{
}

void
WaterSplash::draw ()
{
  float factor = time * 3.0f;
  
  if(factor < .5f)
    sprite.set_scale(1.0f, 3*factor);
  else
    sprite.set_scale(1.0f, 3*(.5f - (factor - 0.5f)));

  sprite.draw((int)pos.x, (int)pos.y);
}

void
WaterSplash::update (float delta)
{
  time += delta;

  if (time > 1.0f/3)
    remove();
}

/* EOF */
