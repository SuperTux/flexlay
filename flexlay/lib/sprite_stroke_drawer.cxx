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
#include <assert.h>
#include <ClanLib/Display/blend_func.h>
#include <ClanLib/Display/canvas.h>
#include <ClanLib/gl.h>
#include <ClanLib/GL/opengl_wrap.h>
#include "stroke.hxx"
#include "flexlay.hxx"
#include "stroke_drawer_impl.hxx"
#include "sprite_stroke_drawer.hxx"
#include "drawer_properties.hxx"
#include "bitmap_layer.hxx"
#include "sketch_layer.hxx"

CL_ProgramObject* program = 0;

class SpriteStrokeDrawerImpl : public StrokeDrawerImpl
{
public:
  SpriteStrokeDrawer::DrawMode mode;
  
  SpriteStrokeDrawerImpl() {}

  void draw(const Stroke& stroke, CL_GraphicContext* gc);

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
  impl->mode      = SpriteStrokeDrawer::DM_NORMAL;
}

void
SpriteStrokeDrawerImpl::draw(const Stroke& stroke, CL_GraphicContext* gc)
{
  if (DrawerProperties::current()->get_brush().is_null() || stroke.get_dab_count() == 0)
    return;
  
  Stroke::Dabs dabs = stroke.get_interpolated_dabs(DrawerProperties::current()->get_spacing()
                                                   * DrawerProperties::current()->get_size(),
                                                   DrawerProperties::current()->get_spacing()
                                                   * DrawerProperties::current()->get_size());

  for(Stroke::Dabs::iterator i = dabs.begin(); i != dabs.end(); ++i)
    {
      Dab& dab = *i;

      CL_Sprite sprite = DrawerProperties::current()->get_brush().get_sprite();

      CL_Color color = DrawerProperties::current()->get_color();
      sprite.set_color(color);
      sprite.set_alpha((color.get_alpha()/255.0f) * dab.pressure);
      sprite.set_scale(DrawerProperties::current()->get_size() * dab.pressure,
                       DrawerProperties::current()->get_size() * dab.pressure);

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
              sprite.set_blend_func_separate(blend_src_alpha, blend_one_minus_src_alpha,
                                             blend_one, blend_one_minus_src_alpha);
              sprite.draw(dab.pos.x, dab.pos.y, gc);
              break;

            case SpriteStrokeDrawer::DM_ADDITION:
              sprite.set_blend_func_separate(blend_src_alpha, blend_one,
                                             blend_zero, blend_one);
              //blend_one, blend_one_minus_src_alpha);
              sprite.draw(dab.pos.x, dab.pos.y, gc);
              break;
              
            case SpriteStrokeDrawer::DM_ERASE:
              sprite.set_blend_func(blend_zero, blend_one_minus_src_alpha);
              sprite.draw(dab.pos.x, dab.pos.y, gc);
              break;
          
            case SpriteStrokeDrawer::DM_SMUDGE:
              {
                if (i != dabs.begin())
                  {
                    CL_Canvas* canvas = BitmapLayer::current()->get_canvas();
                    CL_PixelBuffer buffer = canvas->get_pixeldata(CL_Rect(CL_Point(static_cast<int>((i-1)->pos.x) - sprite.get_width()/2,
                                                                                   static_cast<int>((i-1)->pos.y) - sprite.get_height()/2),
                                                                          CL_Size(sprite.get_width(), sprite.get_height())));
                    CL_Surface surface(buffer);
                    //surface.set_blend_func_separate(blend_src_alpha, blend_one_minus_src_alpha,
                    //                                blend_one, blend_zero);
                    surface.set_alignment(origin_center);
                    surface.set_alpha(0.5);
                    //surface.set_scale(DrawerProperties::current()->get_size(),
                    //                 DrawerProperties::current()->get_size());
                    surface.draw(dab.pos.x, dab.pos.y, gc);
                  }
              }
              break;

            case SpriteStrokeDrawer::DM_SHADER:
              {
#if 0 
                CL_OpenGLState state(gc);
                state.set_active();
                state.setup_2d();

                if (program == 0)
                  {
                    program = new CL_ProgramObject();
                
                    CL_ShaderObject shader("shader", &(Flexlay::current()->resources));
                    std::cout << "Shader status: " << (shader.get_compile_status() ? "true" : "false") << std::endl;
                    std::cout << "Shader log: " << shader.get_info_log() << std::endl;
                    std::cout << "Shader handle: " << shader.get_handle() << std::endl;

                    program->attach(shader);
                    program->link();
                    std::cout << "Program status: " << (program->get_link_status() ? "true" : "false") << std::endl;
                    std::cout << "Program log: " << program->get_info_log() << std::endl;
                    std::cout << "Program handle: " << program->get_handle() << std::endl;

                    clUseProgram(program->get_handle());
                  }
                else
                  {
                    clUseProgram(program->get_handle());
                  }
            
                CL_OpenGLSurface glsurface(sprite.get_frame_surface(0));
                glActiveTexture(GL_TEXTURE0);
                glsurface.bind();
                glEnable(GL_TEXTURE_2D);

                /*CL_OpenGLSurface glsurface2(SketchLayer::current()->get_background_surface());
                  glActiveTexture(GL_TEXTURE1);
                  glsurface2.bind();
                  glEnable(GL_TEXTURE_2D);*/
            
                clUniform1i(program->get_attribute_location("mytex"), 0);
                //clUniform1i(program->get_attribute_location("background"), 1);
                //program->validate();
                //std::cout << "Program validate status: " << (program->get_validate_status() ? "true" : "false") << std::endl;
                //std::cout << "Program log: " << program->get_info_log() << std::endl;

                clBegin(CL_QUADS);
                clColor4b(color.get_red(), color.get_green(), color.get_blue(), color.get_alpha());
                float size = base_size * dab.pressure;
                clVertex2f((dab.pos.x - sprite.get_width()/2) * size, (dab.pos.y - sprite.get_height()/2) * size);
                clTexCoord2d(0.0, 0.0);
                clVertex2f((dab.pos.x + sprite.get_width()/2) * size, (dab.pos.y - sprite.get_height()/2) * size);
                clTexCoord2d(1.0, 0.0);
                clVertex2f((dab.pos.x + sprite.get_width()/2) * size, (dab.pos.y + sprite.get_height()/2) * size);
                clTexCoord2d(1.0, 1.0);
                clVertex2f((dab.pos.x - sprite.get_width()/2) * size, (dab.pos.y + sprite.get_height()/2) * size);
                clTexCoord2d(0.0, 1.0);
                clEnd();
            
                state.set_active();
                clUseProgram(0);
#endif
              }
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
              sprite.set_blend_func(blend_src_alpha, blend_one_minus_src_alpha);
              sprite.draw(dab.pos.x, dab.pos.y, gc);
              break;
              
            case SpriteStrokeDrawer::DM_ADDITION:
              sprite.set_blend_func(blend_src_alpha, blend_one);
              sprite.draw(dab.pos.x, dab.pos.y, gc); 
              break;
            
            case SpriteStrokeDrawer::DM_ERASE:
              sprite.set_blend_func(blend_zero, blend_one_minus_src_alpha);
              sprite.draw(dab.pos.x, dab.pos.y, gc);
              break; 
          
            case SpriteStrokeDrawer::DM_SMUDGE:
              sprite.set_blend_func(blend_src_alpha, blend_one_minus_src_alpha);
              sprite.draw(dab.pos.x, dab.pos.y, gc);          
              break;

            default:
              std::cout << "Error: SpriteStrokeDrawer: Unknown draw mode: " << mode << std::endl;
              break;
            }
        }
    }
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
