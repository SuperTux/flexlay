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

#include <iostream>
#include "stroke.hxx"
#include "stroke_drawer.hxx"

class StrokeImpl
{
public:
  Stroke::Dabs dabs;
  StrokeDrawer drawer;

  // Additional data which should be moved to the StrokeDrawer, since
  // its for caching only and can be generated
  //typedef std::vector<CL_Pointf> Normals;
  //Normals normals;
};

Stroke::Stroke() 
  : impl(new StrokeImpl())
{
}

void
Stroke::set_drawer(const StrokeDrawer& drawer_)
{
  impl->drawer = drawer_;
}

Stroke::Dabs
Stroke::get_dabs() const
{
  return impl->dabs; 
}

void 
Stroke::draw()
{
  if (!impl->drawer.is_null())
    {
      impl->drawer.draw(*this);
    }
  else
    {
      std::cout << "No drawer set!" << std::endl;
    }
}

void
Stroke::add_dab(const Dab& dab) 
{
  impl->dabs.push_back(dab);
}

/* // calc normals
  assert(normals.size() == 0);

  if (points.size() == 1)
    {
      normals.push_back(CL_Pointf(1.0f, 1.0f));
    }
  else if (points.size() == 2)
    {
      normals.push_back(CL_Pointf(1.0f, 1.0f));
      normals.push_back(CL_Pointf(1.0f, 1.0f));
    }
  else if (points.size() >= 3)
    {
      for(Points::size_type i = 0; i < int(points.size())-1; ++i)
        {
          CL_Pointf normal((points[i].y - points[i+1].y),
                           -(points[i].x - points[i+1].x));

          float length = sqrt(normal.x * normal.x + normal.y * normal.y);

          normal.x /= length;
          normal.y /= length;
          
          normals.push_back(normal);
        }
      
      normals.push_back(CL_Pointf(1.0f, 1.0f));
    }

  //std::cout << normals.size() << " == " <<  points.size() << std::endl;
  assert(normals.size() == points.size());

 */

/*
  // Calc bounding rect
  if (points.size() >= 1)
    {
      bounding_rect.left = bounding_rect.right  = points.front().x;
      bounding_rect.top  = bounding_rect.bottom = points.front().y;

      for(Points::iterator i = points.begin()+1; i != points.end(); ++i)
        {
          bounding_rect.left   = Math::min(bounding_rect.left,   i->x);
          bounding_rect.right  = Math::max(bounding_rect.right,  i->x);;
          bounding_rect.top    = Math::min(bounding_rect.top,    i->y);
          bounding_rect.bottom = Math::min(bounding_rect.bottom, i->y);
        }

      // FIXME: Need to take brush size into account
    }
*/

/* EOF */
