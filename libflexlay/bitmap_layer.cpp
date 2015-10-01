// Flexlay - A Generic 2D Game Editor
// Copyright (C) 2002 Ingo Ruhnke <grumbel@gmail.com>
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <http://www.gnu.org/licenses/>.

#include "bitmap_layer.hpp"

#include <assert.h>

#include "color.hpp"
#include "graphic_context.hpp"
#include "objmap_object_impl.hpp"
#include "pixel_buffer.hpp"
#include "pixel_buffer.hpp"
#include "surface.hpp"

class CL_Canvas;

BitmapLayer* BitmapLayer::current_ = 0;

class BitmapLayerImpl : public ObjMapObjectImpl
{
public:
  typedef std::vector<Stroke> Strokes;
  /** All strokes done on this image are recorded for possible later
      playback on a larger size canvas */
  Strokes strokes;

  /** Used to cache drawings */
  Surface  surface;
  CL_Canvas*  canvas;
  Pointf   last_pos;

  BitmapLayerImpl(Surface surface_) :
    surface(surface_),
    canvas(0)
  {
#ifdef GRUMBEL
    try {
      canvas = new CL_Canvas(surface.to_cl());
      canvas->sync_surface();
    } catch(const CL_Error& err) {
      std::cout << "CL_Error: " << err.message << std::endl;
      throw err;
    }
#endif
  }

  BitmapLayerImpl(PixelBuffer buffer)
    : surface(buffer),
      canvas(0)
  {
#ifdef GRUMBEL
    try {
      canvas = new CL_Canvas(surface.to_cl());
      canvas->sync_surface();
    } catch(const CL_Error& err) {
      std::cout << "CL_Error: " << err.message << std::endl;
      throw err;
    }
#endif
  }

  BitmapLayerImpl(int width, int height)
    : surface(PixelBuffer(width, height)),
      canvas(0)
  {
#ifdef GRUMBEL
    try {
      canvas = new CL_Canvas(surface.to_cl());
      canvas->get_gc()->clear(Color(0, 0, 0, 0).to_cl());
      canvas->get_gc()->flush();
      canvas->sync_surface();
    } catch(const CL_Error& err) {
      std::cout << "CL_Error: " << err.message << std::endl;
      throw err;
    }
#endif
  }

  ~BitmapLayerImpl() {
#ifdef GRUMBEL
    delete canvas;
#endif
  }

  void draw(GraphicContext& gc)
  {
    assert(canvas);

    // Nothing to draw, so we go byebye
    if (strokes.empty())
      return;

    surface.set_blend_func(BlendFunc::one, BlendFunc::one_minus_src_alpha);
    surface.draw(pos.x, pos.y);

    gc.draw_rect(get_bounding_rect(), Color(155, 155, 155, 100));
  }

  Rectf get_bound_rect() const
  {
    return Rectf(Pointf(ObjMapObjectImpl::pos), Sizef(surface.get_width(), surface.get_height()));
  }

  Rect get_bounding_rect() {
    // FIXME: Do we need to handle its position here or does the Layer keep care of that?
    return Rect(Point(0, 0),
                Size(surface.get_width(), surface.get_height()));
  }

  bool has_bounding_rect() const {
    return true;
  }
};

BitmapLayer::BitmapLayer(Surface surface)
  : impl(new BitmapLayerImpl(surface))
{
  current_ = this;
}

BitmapLayer::BitmapLayer(int width, int height)
  : impl(new BitmapLayerImpl(width, height))
{
  current_ = this;
}

BitmapLayer::BitmapLayer(PixelBuffer buffer)
  : impl(new BitmapLayerImpl(buffer))
{
  current_ = this;
}

void
BitmapLayer::add_stroke(const Stroke& stroke)
{
#ifdef GRUMBEL
  if (stroke.get_dab_count() > 0)
  {
    impl->strokes.push_back(stroke);
    stroke.draw(impl->canvas->get_gc());
    // FIXME: doesn't sync when manually manipulating the canvas
    impl->canvas->get_gc()->flush();
    impl->canvas->sync_surface();
  }
#endif
}

std::vector<Stroke>
BitmapLayer::get_strokes()
{
  return impl->strokes;
}

Surface
BitmapLayer::get_background_surface()
{
  return impl->surface;
}

CL_Canvas*
BitmapLayer::get_canvas() const
{
  return impl->canvas;
}

void
BitmapLayer::set_pixeldata(PixelBuffer buffer)
{
#ifdef GRUMBEL
  //impl->canvas->set_pixeldata(buffer);
  Surface(buffer).draw(0, 0, impl->canvas->get_gc());
  impl->canvas->get_gc()->flush();
  impl->canvas->sync_surface();
#endif
}

PixelBuffer
BitmapLayer::get_pixeldata() const
{
#ifdef GRUMBEL
  return impl->canvas->get_pixeldata();
#endif
  return PixelBuffer();
}

ObjMapObject
BitmapLayer::to_object()
{
  return ObjMapObject(impl);
}

/* EOF */
