/*  $Id$
**   __      __ __             ___        __   __ __   __
**  /  \    /  \__| ____    __| _/_______/  |_|__|  | |  |   ____
**  \   \/\/   /  |/    \  / __ |/  ___/\   __\  |  | |  | _/ __ \
**   \        /|  |   |  \/ /_/ |\___ \  |  | |  |  |_|  |_\  ___/
**    \__/\  / |__|___|  /\____ /____  > |__| |__|____/____/\___  >
**         \/          \/      \/    \/                         \/
**  Copyright (C) 2005 Ingo Ruhnke <grumbel@gmx.de>
**
**  This program is free software; you can redistribute it and/or
**  modify it under the terms of the GNU General Public License
**  as published by the Free Software Foundation; either version 2
**  of the License, or (at your option) any later version.
**
**  This program is distributed in the hope that it will be useful,
**  but WITHOUT ANY WARRANTY; without even the implied warranty of
**  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
**  GNU General Public License for more details.
** 
**  You should have received a copy of the GNU General Public License
**  along with this program; if not, write to the Free Software
**  Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
**  02111-1307, USA.
*/

#include <math.h>
#include "stroke.hpp"

Stroke::Stroke()
{
  
}

Stroke::~Stroke()
{
  
}

Stroke::Dabs
Stroke::get_interpolated_dabs(float x_spacing, float y_spacing) const
{
  if (dabs.size() > 0)
    {
      Dabs interpolated_dabs;

      interpolated_dabs.push_back(dabs.front());

      // The following code basically takes all the event dabs as recieved
      // by from the InputDevice and interpolates new dabs inbetween to
      // give them an equal spacing (ie. every dab is only 'spacing' away
      // from the next)
      float overspace = 0.0f;
      for(unsigned int j = 0; j < dabs.size()-1; ++j)
        {
          Vector dist = dabs[j+1].pos - dabs[j].pos;
          float length = sqrt(dist.x * dist.x + dist.y * dist.y);
          int n = 1;
    
          // Spacing is keep relative to the brush size
          // FIXME: This is specific to a Sprite based drawer, might not work for others
          // FIXME: y_spacing isn't taken into account either
          float local_spacing = x_spacing * dabs[j].pressure;

          while (length + overspace > (local_spacing * n))
            {
              float factor = (local_spacing/length) * n - (overspace/length);
          
              // FIXME: Interpolate tilting, pressure, etc. along the line
              interpolated_dabs.push_back(Dab(dabs[j].pos.x + dist.x * factor,
                                              dabs[j].pos.y + dist.y * factor,
                                              dabs[j].pressure));
              n += 1;
            }

          // calculate the space that wasn't used in the last iteration
          overspace = (length + overspace) - (local_spacing * (n-1));
        }
      return interpolated_dabs;
    }
  else
    {
      // No dabs available, so nothing to interpolate
      return dabs;
    }
}

const Stroke::Dabs&
Stroke::get_dabs() const
{
  return dabs; 
}

int
Stroke::get_dab_count() const
{
  return dabs.size();
}

void
Stroke::add_dab(const Dab& dab)
{
  if(dabs.empty())
    {
      bounding_rect = Rect(Point(static_cast<int>(dab.pos.x),
                                 static_cast<int>(dab.pos.y)),
                           Size(1, 1));
    }
  else
    {
      bounding_rect.left    = std::min(bounding_rect.left,   static_cast<int>(dab.pos.x));
      bounding_rect.top     = std::min(bounding_rect.top,    static_cast<int>(dab.pos.y));

      bounding_rect.right   = std::max(bounding_rect.right,  static_cast<int>(dab.pos.x));
      bounding_rect.bottom  = std::max(bounding_rect.bottom, static_cast<int>(dab.pos.y));
    }

  dabs.push_back(dab);
}

const Rect&
Stroke::get_bounding_rect() const
{
  return bounding_rect; 
}

/* EOF */
