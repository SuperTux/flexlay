/*            _   ___              _   
**   _ _  ___| |_| _ )_ _ _  _ _ _| |_ 
**  | ' \/ -_)  _| _ \ '_| || (_-<|   |
**  |_||_\___|\__|___/_|  \_,_/__/|_|_|
**  netBrush - Copyright (C) 2006 Ingo Ruhnke <grumbel@gmx.de>
**
**  This program is free software: you can redistribute it and/or modify
**  it under the terms of the GNU General Public License as published by
**  the Free Software Foundation, either version 3 of the License, or
**  (at your option) any later version.
**  
**  This program is distributed in the hope that it will be useful,
**  but WITHOUT ANY WARRANTY; without even the implied warranty of
**  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
**  GNU General Public License for more details.
**  
**  You should have received a copy of the GNU General Public License
**  along with this program.  If not, see <http://www.gnu.org/licenses/>.
*/

#ifndef HEADER_GENERIC_BRUSH_HPP
#define HEADER_GENERIC_BRUSH_HPP

#include "brushmask.hpp"

class GenericBrush
{
public:
  BrushShape shape;
  float      radius;
  int        spikes;        /* 2 - 20     */
  float      hardness;      /* 0.0 - 1.0  */
  float      aspect_ratio;  /* y/x (1.0f - 20.0f) */
  float      angle;         /* in degrees */
  
  GenericBrush()
    : shape(BRUSH_SHAPE_CIRCLE),
      radius(10),
      spikes(2),
      hardness(0.5),
      aspect_ratio(1.0f),
      angle(0.0f)
  {}

  GrayscaleBuffer* generate() const;
};

#endif

/* EOF */
