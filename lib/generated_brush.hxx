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

#ifndef HEADER_GENERATED_BRUSH_HXX
#define HEADER_GENERATED_BRUSH_HXX

#include "shared_ptr.hxx"
#include "brushmask.hxx"
#include "brush.hxx"

class GeneratedBrushImpl;

/** */
class GeneratedBrush
{
private:
public:
  GeneratedBrush(const Brush& brush);
  GeneratedBrush(BrushShape shape,
                 float  radius,
                 int    spikes,        /* 2 - 20     */
                 float  hardness,      /* 0.0 - 1.0  */
                 float  aspect_ratio,  /* y/x        */
                 float  angle);

  void set_shape(BrushShape shape);
  BrushShape get_shape();

  void  set_radius(float radius);
  float get_radius();

  void set_spikes(int spikes);
  int  get_spikes();

  void  set_hardness(float hardness);
  float get_hardness();

  void  set_aspect_ratio(float aspect);
  float get_aspect_ratio();

  void  set_angle(float angle);
  float get_angle();

  Brush to_brush();
private:
  SharedPtr<GeneratedBrushImpl> impl;
};

#endif

/* EOF */
