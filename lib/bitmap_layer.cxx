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
#include <ClanLib/gl.h>
#include <ClanLib/Core/core_iostream.h>
#include <ClanLib/Core/System/error.h>
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
#include "bitmap_layer.hxx"
#include "objmap_object_impl.hxx"
#include "math.hxx"

BitmapLayer* BitmapLayer::current_ = 0;

class BitmapLayerImpl : public ObjMapObjectImpl
{
public:
  typedef std::vector<Stroke> Strokes;
  /** All strokes done on this image are recorded for possible later
      playback on a larger size canvas */
  Strokes strokes;

  /** Used to cache drawings */
  CL_Surface  surface;
  CL_Canvas*  canvas;
  CL_Pointf   last_pos;

  BitmapLayerImpl(CL_Surface surface_)
    : surface(surface_),
      canvas(0)
  {
    try {
      canvas = new CL_Canvas(surface);
      canvas->sync_surface();
    } catch(CL_Error& err) {
      std::cout << "CL_Error: " << err.message << std::endl;
      throw err;
    }
  }
  
  BitmapLayerImpl(CL_PixelBuffer buffer)
    : surface(buffer),
      canvas(0)
  {
    try {
      canvas = new CL_Canvas(surface);
      canvas->sync_surface();
    } catch(CL_Error& err) {
      std::cout << "CL_Error: " << err.message << std::endl;
      throw err;
    }
  }

  BitmapLayerImpl(int width, int height) 
    : surface(CL_PixelBuffer(width, height, width*4, CL_PixelFormat::rgba8888)),
      canvas(0)
  {
    try {
      canvas = new CL_Canvas(surface);
      canvas->get_gc()->clear(CL_Color(0, 0, 0, 0));
      canvas->get_gc()->flush();
      canvas->sync_surface();
    } catch(CL_Error& err) {
      std::cout << "CL_Error: " << err.message << std::endl;
      throw err;
    }
  }

  ~BitmapLayerImpl() {
    delete canvas;
  }

  void draw(CL_GraphicContext* gc)
  {
    assert(canvas);

    // Nothing to draw, so we go byebye
    if (strokes.empty()) 
      return;

    surface.set_blend_func(blend_one, blend_one_minus_src_alpha);
    surface.draw(pos.x, pos.y, gc);

    gc->draw_rect(get_bounding_rect(), CL_Color(155, 155, 155, 100));
  }

  CL_Rectf get_bound_rect() const  
  {
    return CL_Rectf(CL_Pointf(ObjMapObjectImpl::pos), CL_Sizef(surface.get_width(), surface.get_height()));
  }

  CL_Rect get_bounding_rect() { 
    // FIXME: Do we need to handle its position here or does the Layer keep care of that?
    return CL_Rect(CL_Point(0, 0),
                   CL_Size(surface.get_width(), surface.get_height())); 
  }

  bool has_bounding_rect() const { 
    return true;
  }
};

BitmapLayer::BitmapLayer(CL_Surface surface)
  : impl(new BitmapLayerImpl(surface))
{
  current_ = this;
}

BitmapLayer::BitmapLayer(int width, int height)
  : impl(new BitmapLayerImpl(width, height))
{
  current_ = this;
}

BitmapLayer::BitmapLayer(CL_PixelBuffer buffer)
  : impl(new BitmapLayerImpl(buffer))
{
  current_ = this;
}

void
BitmapLayer::add_stroke(const Stroke& stroke)
{
  if (stroke.get_dab_count() > 0)
    {
      impl->strokes.push_back(stroke);
      stroke.draw(impl->canvas->get_gc());
      // FIXME: doesn't sync when manually manipulating the canvas
      impl->canvas->get_gc()->flush();
      impl->canvas->sync_surface();
    }
}

std::vector<Stroke>
BitmapLayer::get_strokes()
{
  return impl->strokes;
}

CL_Surface
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
BitmapLayer::set_pixeldata(CL_PixelBuffer buffer)
{
  //impl->canvas->set_pixeldata(buffer);
  CL_Surface(buffer).draw(0, 0, impl->canvas->get_gc());
  impl->canvas->get_gc()->flush();
  impl->canvas->sync_surface();
}

CL_PixelBuffer
BitmapLayer::get_pixeldata() const
{
  return impl->canvas->get_pixeldata();
}

ObjMapObject
BitmapLayer::to_object()
{
  return ObjMapObject(impl);
}

/* EOF */
