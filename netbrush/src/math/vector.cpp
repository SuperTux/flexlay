//  $Id: vector.cpp 2642 2005-06-26 13:38:53Z matzebraun $
//
//  SuperTux -  A Jump'n Run
//  Copyright (C) 2004 Matthias Braun <matze@braunis.de
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

#include <cmath>
#include <iostream>
#include "vector.hpp"

void
Vector::normalize()
{
  float mag = magnitude();
  x /= mag;
  y /= mag;
}

Vector Vector::unit() const
{
  return *this / magnitude();
}

float
Vector::magnitude() const
{
  return sqrt(x*x + y*y);
}

std::ostream& operator<<(std::ostream& s, const Vector& v)
{
  s << "(" << v.x << ", " << v.y << ")";
  return s;
}

Vector
Vector::rotate(float angle) const
{
  float len = magnitude();
  return Vector(len * cos(angle), len * sin(angle));
}

/* EOF */
