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
#include <ClanLib/gl.h>
#include <ClanLib/Core/core_iostream.h>
#include <ClanLib/Display/display.h>
#include <ClanLib/Display/sprite.h>
#include <ClanLib/Display/pixel_buffer.h>
#include <ClanLib/Display/canvas.h>
#include <ClanLib/Display/blend_func.h>
#include <ClanLib/Display/graphic_context.h>
#include <ClanLib/Display/display_window.h>
#include "flexlay.hxx"
#include "editor_map_component.hxx"
#include "workspace.hxx"
#include "layer_impl.hxx"
#include "sketch_layer.hxx"
#include "math.hxx"

SketchLayer* SketchLayer::current_ = 0;

class SketchLayerImpl : public LayerImpl
{
public:
  typedef std::vector<Stroke> Strokes;
  Strokes strokes;

  /** Used to cache drawings */
  CL_Surface surface;
  CL_Canvas  canvas;
  float     last_zoom;
  CL_Pointf last_pos;
  
  SketchLayerImpl() 
    : surface(new CL_PixelBuffer(800, 600, 800*4, CL_PixelFormat::rgb888), true),
      canvas(surface),
      last_zoom(0.0f)
  {
    
  }

  void add_stroke(const Stroke& stroke)
  {
    strokes.push_back(stroke);

    EditorMapComponent* parent = EditorMapComponent::current();
    parent->get_workspace().get_gc_state().push(canvas.get_gc());
    stroke.draw(canvas.get_gc());
    parent->get_workspace().get_gc_state().pop(canvas.get_gc());
    canvas.sync_surface();
  }
  
  void draw(EditorMapComponent* parent) 
  {
    if (1)
      {
        if (last_zoom != parent->get_workspace().get_gc_state().get_zoom() ||
            last_pos  != parent->get_workspace().get_gc_state().get_pos())
          {
            // Rerender the image
            last_zoom   = parent->get_workspace().get_gc_state().get_zoom();
            last_pos    = parent->get_workspace().get_gc_state().get_pos();

            parent->get_workspace().get_gc_state().push(canvas.get_gc());
            //canvas.get_gc()->clear(CL_Color(0, 0, 0, 0));
            canvas.get_gc()->clear(CL_Color::white);
            for(Strokes::iterator i = strokes.begin(); i != strokes.end(); ++i)
              {
                i->draw(canvas.get_gc());
              }
            parent->get_workspace().get_gc_state().pop(canvas.get_gc());

            canvas.sync_surface();
          }
        
        surface.set_blend_func(blend_src_alpha, blend_one_minus_src_alpha);

        CL_Matrix4x4 matrix = CL_Display::get_modelview();
        CL_Display::pop_modelview();
        surface.draw();
        CL_Display::set_modelview(matrix);
        CL_Display::push_modelview();
      }
    else
      {
        for(Strokes::iterator i = strokes.begin(); i != strokes.end(); ++i)
          {
            i->draw(0);
          }
      }
  }

  bool has_bounding_rect() const { 
    return false;
  }
};

SketchLayer::SketchLayer()
  : impl(new SketchLayerImpl())
{
  current_ = this;
}

void
SketchLayer::add_stroke(const Stroke& stroke)
{
  if (stroke.get_dab_count() > 0)
    impl->add_stroke(stroke);
}

Layer
SketchLayer::to_layer()
{
   return Layer(impl);
}

std::vector<Stroke>
SketchLayer::get_strokes()
{
  return impl->strokes;
}

/* EOF */
