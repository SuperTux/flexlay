//  $Id$
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

#ifndef HEADER_STROKE_HXX
#define HEADER_STROKE_HXX

#include <vector>
#include <ClanLib/Core/Math/point.h>
#include "shared_ptr.hxx"

class StrokeImpl;
class StrokeDrawer;
class CL_GraphicContext;

class Dab
{
public:
  /** Time at which the dot was placed */
  unsigned int time;

  /** Position at which the dot is placed */
  CL_Pointf pos;

  /** The pressure with which the dot was drawn (can be interpreted as
      size, opacity or similar things by the StrokeDrawer */
  float pressure;

  /** Tilting of the pen while painting the dot */
  CL_Pointf tilt;

  Dab()
    : time(0), pos(0, 0), pressure(1.0f), tilt(0, 0)
  {}

  Dab(float x, float y) 
    : time(0), pos(x, y), pressure(1.0f), tilt(0.0f, 0.0f)
  {}
};

class Stroke
{
public:
  typedef std::vector<Dab> Dabs;

  Stroke();

  void draw(CL_GraphicContext* gc) const;

  void draw_pass1(CL_GraphicContext* gc) const;
  void draw_pass2(CL_GraphicContext* gc) const;

  void  set_drawer(const StrokeDrawer& drawer_);
  void  add_dab(const Dab& dab);
  Dabs  get_dabs()  const;

  int get_dab_count() const;
private:
  SharedPtr<StrokeImpl> impl;
};


#endif

/* EOF */
