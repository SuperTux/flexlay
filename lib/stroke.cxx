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

  mutable bool bounding_rect_needs_recalc;
  mutable CL_Rectf bounding_rect;

  CL_Rectf calc_bounding_rect() const
  {
    CL_Rectf rect;

    // FIXME: Keep the drawer into account (ie. brushsize)
    if (dabs.size() > 0)
      {
        rect.left = rect.right  = dabs.front().pos.x;
        rect.top  = rect.bottom = dabs.front().pos.y;

        for(Stroke::Dabs::const_iterator i = dabs.begin()+1; i != dabs.end(); ++i)
          {
            rect.left = std::min(i->pos.x, rect.left);
            rect.top  = std::min(i->pos.y, rect.top);

            rect.right  = std::max(i->pos.x, rect.right);
            rect.bottom = std::max(i->pos.y, rect.bottom);
          }
      }
    
    return rect;
  }

  StrokeImpl() 
    : bounding_rect_needs_recalc(true)
  {
  }
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

StrokeDrawer
Stroke::get_drawer()
{
  return impl->drawer;
}

Stroke::Dabs
Stroke::get_interpolated_dabs(float x_spacing, float y_spacing) const
{
  if (impl->dabs.size() > 0)
    {
      Dabs interpolated_dabs;

      interpolated_dabs.push_back(impl->dabs.front());

      // The following code basically takes all the event dabs as recieved
      // by from the InputDevice and interpolates new dabs inbetween to
      // give them an equal spacing (ie. every dab is only 'spacing' away
      // from the next)
      float overspace = 0.0f;
      const Stroke::Dabs& dabs = impl->dabs;
      for(unsigned int j = 0; j < dabs.size()-1; ++j)
        {
          CL_Pointf dist = dabs[j+1].pos - dabs[j].pos;
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
      return impl->dabs;
    }
}

Stroke::Dabs
Stroke::get_dabs() const
{
  return impl->dabs; 
}

int
Stroke::get_dab_count() const
{
  return impl->dabs.size();
}

void 
Stroke::draw(CL_GraphicContext* gc) const
{
  if (!impl->drawer.is_null())
    {
      const_cast<StrokeDrawer&>(impl->drawer).draw(*this, gc);
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

CL_Rectf
Stroke::get_bounding_rect() const
{
  if (impl->bounding_rect_needs_recalc)
    {
      impl->bounding_rect = impl->calc_bounding_rect();
      impl->bounding_rect_needs_recalc = false;
    }
  
  return impl->bounding_rect;
}

bool
Stroke::empty() const
{
  return (impl->dabs.size() == 0);
}

/* EOF */
