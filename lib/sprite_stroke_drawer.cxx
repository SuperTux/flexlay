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
#include "stroke_drawer_impl.hxx"
#include "sprite_stroke_drawer.hxx"

class SpriteStrokeDrawerImpl : public StrokeDrawerImpl
{
public:
  CL_Color  color;
  float     base_size;
  float     spacing;
  CL_Sprite brush;

  
  void draw(const Stroke& stroke);
  StrokeDrawerImpl* clone() const;
};

SpriteStrokeDrawer::SpriteStrokeDrawer()
  : impl(new SpriteStrokeDrawerImpl())
{
  impl->base_size = 1.0f;
  impl->spacing   = 20.0f;
}

void
SpriteStrokeDrawerImpl::draw(const Stroke& stroke) 
{
  if (brush.is_null())
    return;
    
  // Spacing is keep relative to the brush size
  float local_spacing = spacing * base_size;

  brush.set_color(color);
  brush.set_scale(base_size, base_size);
  
  if (stroke.get_dabs().size() == 1 || stroke.get_dabs().size() == 2)
    { // FIXME: More or less a hack
      brush.draw(stroke.get_dabs().front().pos.x, stroke.get_dabs().front().pos.y);
    }
  else
    {
      brush.draw(stroke.get_dabs().front().pos.x, stroke.get_dabs().front().pos.y);

      float overspace = 0.0f;
      Stroke::Dabs dabs = stroke.get_dabs();
      for(unsigned int j = 0; j < dabs.size()-1; ++j)
        {
          CL_Pointf dist = dabs[j+1].pos - dabs[j].pos;
          float length = sqrt(dist.x * dist.x + dist.y * dist.y);
          int n = 1;
          
          while (length + overspace > (local_spacing * n))
            {
              float factor = (local_spacing/length) * n - (overspace/length);
              CL_Pointf p(dabs[j].pos.x + dist.x * factor,
                          dabs[j].pos.y + dist.y * factor);

              brush.set_color(color);
              brush.set_scale(base_size, base_size);
  
              brush.draw(p.x, p.y);
              
              n += 1;
            }

          // calculate the space that wasn't used in the last iteration
          overspace = (length + overspace) - (local_spacing * (n-1));
        }
    }
}

void
SpriteStrokeDrawer::set_spacing(float spacing_)
{
  impl->spacing = spacing_;
}

float
SpriteStrokeDrawer::get_spacing() const
{
  return impl->spacing;
}

void
SpriteStrokeDrawer::set_size(float s)
{
  impl->base_size = s;
}

float
SpriteStrokeDrawer::get_size() const
{
  return impl->base_size;
}

void
SpriteStrokeDrawer::set_color(const CL_Color& color_)
{
  impl->color = color_;
}

CL_Color
SpriteStrokeDrawer::get_color() const
{
  return impl->color;
}

void
SpriteStrokeDrawer::set_sprite(const CL_Sprite& sprite_)
{
  impl->brush = sprite_;
}

CL_Sprite
SpriteStrokeDrawer::get_sprite() const
{
  return impl->brush;
}

StrokeDrawerImpl*
SpriteStrokeDrawerImpl::clone() const
{
  SpriteStrokeDrawerImpl* drawer = new SpriteStrokeDrawerImpl();
  
  *drawer = *this;
  
  return drawer;
}

StrokeDrawer
SpriteStrokeDrawer::to_drawer()
{
  return StrokeDrawer(impl);
}

/* EOF */
