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
#include <ClanLib/Display/blend_func.h>
#include <ClanLib/gl.h>
#include "stroke.hxx"
#include "stroke_drawer_impl.hxx"
#include "sprite_stroke_drawer.hxx"

class SpriteStrokeDrawerImpl : public StrokeDrawerImpl
{
public:
  SpriteStrokeDrawer::DrawMode mode;
  CL_Color  color;
  float     base_size;
  float     spacing;
  CL_Sprite brush;
  
  void draw(const Stroke& stroke, CL_GraphicContext* gc);
  void draw_dab(const Dab& dab, CL_GraphicContext* gc);

  StrokeDrawerImpl* clone() const;
};

SpriteStrokeDrawer::SpriteStrokeDrawer(StrokeDrawer drawer)
{
  // FIXME: THIS WON'T WORK WITH A REAL SMARTPTR!!!!
  impl = dynamic_cast<SpriteStrokeDrawerImpl*>(drawer.impl.get());
  assert(impl.get());
}

SpriteStrokeDrawer::SpriteStrokeDrawer()
  : impl(new SpriteStrokeDrawerImpl())
{
  impl->base_size = 1.0f;
  impl->spacing   = 15.0f;
  impl->mode      = SpriteStrokeDrawer::DM_NORMAL;
}

void
SpriteStrokeDrawerImpl::draw_dab(const Dab& dab, CL_GraphicContext* gc)
{
  brush.set_color(color);
  brush.set_alpha((color.get_alpha()/255.0f) * dab.pressure);
  brush.set_scale(base_size * dab.pressure,
                  base_size * dab.pressure);

  if (gc != 0)
    {
      /* Correct function:
         1: dest
         2: src
             
         R = R1 A1 (1 - A2) + R2 A2
         G = G1 A1 (1 - A2) + G2 A2
         B = B1 A1 (1 - A2) + B2 A2
         A = A1 (1 - A2) + A2

         // This is currently used, leads to premultiplied alpha
         Aout  = Afgd + (1 - Afgd) * Abkg 
         Cout' = Cfgd' + (1 - Afgd) * Cbkg' 
         where
         Cfgd' = Cfgd * Afgd
         Cbkg' = Cbkg * Abkg
         Cout' = Cout * Aout

         Aout = (1 - (1 - Afgd) * (1 - Abkg)) 
         Cout = (Cfgd * Afgd) + (1 - Afgd * Cbkg * Abkg) / Aout 
         where
         Cfgd = red, green, blue of foreground
         Cbkg = red, green, blue of background
         Afgd = alpha of foreground
         Abkg = alpha of background
      */

      // DO Multipass:
      // 1: GL_ZERO, GL_DST_ALPHA
      // 2: GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA
      /*brush.set_blend_func_separate(blend_zero, blend_dst_alpha,
        blend_zero, blend_one);
        brush.draw(dab.pos.x, dab.pos.y, gc);*/
          
      switch (mode)
        {
        case SpriteStrokeDrawer::DM_NORMAL:
          brush.set_blend_func_separate(blend_src_alpha, blend_one_minus_src_alpha,
                                        blend_one, blend_one_minus_src_alpha);
          brush.draw(dab.pos.x, dab.pos.y, gc);
          break;

        case SpriteStrokeDrawer::DM_ADDITION:
          brush.set_blend_func_separate(blend_src_alpha, blend_one,
                                        blend_zero, blend_one);
                                        //blend_one, blend_one_minus_src_alpha);
          brush.draw(dab.pos.x, dab.pos.y, gc);
          break;
              
        case SpriteStrokeDrawer::DM_ERASE:
          brush.set_blend_func(blend_zero, blend_one_minus_src_alpha);
          brush.draw(dab.pos.x, dab.pos.y, gc);
          break;
              
        default:
          std::cout << "Error: SpriteStrokeDrawer: Unknown draw mode: " << mode << std::endl;
          break;
        }
    }
  else
    {
      switch (mode)
        {
        case SpriteStrokeDrawer::DM_NORMAL:  
          brush.set_blend_func(blend_src_alpha, blend_one_minus_src_alpha);
          brush.draw(dab.pos.x, dab.pos.y, gc);  
          break;
              
        case SpriteStrokeDrawer::DM_ADDITION:
          brush.set_blend_func(blend_src_alpha, blend_one);
          brush.draw(dab.pos.x, dab.pos.y, gc); 
          break;
            
        case SpriteStrokeDrawer::DM_ERASE:
          brush.set_blend_func(blend_zero, blend_one_minus_src_alpha);
          brush.draw(dab.pos.x, dab.pos.y, gc);
          break; 

        default:
          std::cout << "Error: SpriteStrokeDrawer: Unknown draw mode: " << mode << std::endl;
          break;
        }
    }
}

void
SpriteStrokeDrawerImpl::draw(const Stroke& stroke, CL_GraphicContext* gc)
{
  if (brush.is_null() || stroke.get_dab_count() == 0)
    return;
    
  draw_dab(stroke.get_dabs().front(), gc);
  
  float overspace = 0.0f;
  Stroke::Dabs dabs = stroke.get_dabs();
  for(unsigned int j = 0; j < dabs.size()-1; ++j)
    {
      CL_Pointf dist = dabs[j+1].pos - dabs[j].pos;
      float length = sqrt(dist.x * dist.x + dist.y * dist.y);
      int n = 1;
    
      // Spacing is keep relative to the brush size
      float local_spacing = spacing * base_size * dabs[j].pressure;

      while (length + overspace > (local_spacing * n))
        {
          float factor = (local_spacing/length) * n - (overspace/length);
          
          // FIXME: Interpolate tilting, pressure, etc. along the line
          draw_dab(Dab(dabs[j].pos.x + dist.x * factor,
                       dabs[j].pos.y + dist.y * factor,
                       dabs[j].pressure),
                   gc);
              
          n += 1;
        }

      // calculate the space that wasn't used in the last iteration
      overspace = (length + overspace) - (local_spacing * (n-1));
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
  impl->brush.set_alignment(origin_center);
}

CL_Sprite
SpriteStrokeDrawer::get_sprite() const
{
  return impl->brush;
}

void
SpriteStrokeDrawer::set_mode(DrawMode mode)
{
  impl->mode = mode;
}

SpriteStrokeDrawer::DrawMode
SpriteStrokeDrawer::get_mode()
{
  return impl->mode;
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
