//  Flexlay - A Generic 2D Game Editor
//  Copyright (C) 2002 Ingo Ruhnke <grumbel@gmx.de>
//
//  This program is free software: you can redistribute it and/or modify
//  it under the terms of the GNU General Public License as published by
//  the Free Software Foundation, either version 3 of the License, or
//  (at your option) any later version.
//  
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//  GNU General Public License for more details.
//  
//  You should have received a copy of the GNU General Public License
//  along with this program.  If not, see <http://www.gnu.org/licenses/>.

#ifndef HEADER_FLEXLAY_GENERATED_BRUSH_HPP
#define HEADER_FLEXLAY_GENERATED_BRUSH_HPP

#include "brushmask.hpp"
#include "brush.hpp"

class GeneratedBrushImpl;

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

  void  set_spikes(int spikes);
  int   get_spikes();

  void  set_hardness(float hardness);
  float get_hardness();

  void  set_aspect_ratio(float aspect);
  float get_aspect_ratio();

  void  set_angle(float angle);
  float get_angle();

  Brush to_brush();
private:
  std::shared_ptr<GeneratedBrushImpl> impl;
};

#endif

/* EOF */
