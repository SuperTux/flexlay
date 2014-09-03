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

#include "sketch_layer.hpp"

#include <iostream>
#include <ClanLib/Core/core_iostream.h>
#include <ClanLib/Core/System/error.h>
#include <ClanLib/Display/display.h>
#include <ClanLib/Display/sprite.h>
#include <ClanLib/Display/pixel_buffer.h>
#include <ClanLib/Display/pixel_format.h>
#include <ClanLib/Display/canvas.h>
#include <ClanLib/Display/blend_func.h>
#include <ClanLib/Display/display_window.h>

#include "color.hpp"
#include "display.hpp"
#include "flexlay.hpp"
#include "graphic_context.hpp"
#include "gui/editor_map_component.hpp"
#include "layer_impl.hpp"
#include "math.hpp"

SketchLayer* SketchLayer::current_ = 0;

class SketchLayerImpl : public LayerImpl
{
public:
  typedef std::vector<Stroke> Strokes;
  Strokes strokes;

  /** Used to cache drawings */
  CL_Surface  surface;
  CL_Canvas*  canvas;
  float       last_zoom;
  float       last_rot;
  Pointf   last_pos;

  SketchLayerImpl() :
    surface(CL_PixelBuffer(CL_Display::get_width(), CL_Display::get_height(),
                           CL_Display::get_width()*4, CL_PixelFormat::rgba8888)),
    canvas(0),
    last_zoom(0.0f),
    last_rot(0)
  {
    try {
      canvas = new CL_Canvas(surface);
    } catch(const CL_Error& err) {
      std::cout << "CL_Error: " << err.message << std::endl;
    }
  }

  ~SketchLayerImpl() {
    delete canvas;
  }

  void add_stroke(const Stroke& stroke)
  {
    strokes.push_back(stroke);

#ifdef GRUMBEL
    if (canvas)
    {
      EditorMapComponent* parent = EditorMapComponent::current();
      parent->get_gc_state().push(canvas->get_gc());
      stroke.draw(canvas->get_gc());
      parent->get_gc_state().pop(canvas->get_gc());
      canvas->sync_surface();
    }
#endif
  }

  void draw(GraphicContext& gc)
  {
    // Nothing to draw, so we go byebye
    if (strokes.empty())
      return;

    if (canvas)
    {
      // Draw to canvas
      if (last_zoom != gc.state.get_zoom() ||
          last_pos  != gc.state.get_pos()  ||
          last_rot  != gc.state.get_rotation())
      {
        // Rerender the image
        last_zoom   = gc.state.get_zoom();
        last_pos    = gc.state.get_pos();
        last_rot    = gc.state.get_rotation();

#ifdef GRUMBEL
        gc.state.push(canvas->get_gc());
        canvas->get_gc()->clear(Color(0, 0, 0, 0).to_cl());
        //canvas->get_gc()->clear(Color::white);

        Rectf visible_area = state.get_clip_rect();

        for(Strokes::iterator i = strokes.begin(); i != strokes.end(); ++i)
        {
          // canvas->get_gc()->draw_rect(i->get_bounding_rect(), Color(0, 255, 0));
          // canvas->get_gc()->flush();

          if (visible_area.is_overlapped(i->get_bounding_rect()))
          {
            i->draw(canvas->get_gc());
          }
        }
        state.pop(canvas->get_gc());
#endif

        canvas->sync_surface();
      }

      surface.set_blend_func(blend_one, blend_one_minus_src_alpha);

      CL_Matrix4x4 matrix = CL_Display::get_modelview();
      Display::pop_modelview();
      surface.draw();
      CL_Display::set_modelview(matrix);
      // FIXME: I think we need the line below, however with it it
      //doesn't work, without it, it does, ClanLib bug or just
      //consfusing function names?
      //Display::push_modelview();
    }
    else
    {
#ifdef GRUMBEL
      // Direct Drawing, slow
      for(Strokes::iterator i = strokes.begin(); i != strokes.end(); ++i)
      {
        i->draw(0);
      }
#endif
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

CL_Surface
SketchLayer::get_background_surface()
{
  return impl->surface;
}

/* EOF */
