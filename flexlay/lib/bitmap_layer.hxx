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

#ifndef HEADER_BITMAP_LAYER_HXX
#define HEADER_BITMAP_LAYER_HXX

#include <vector>
#include <ClanLib/Core/Math/point.h>
#include <ClanLib/Display/color.h>
#include "objmap_object.hxx"
#include "layer.hxx"
#include "stroke.hxx"

class BitmapLayerImpl;

/** This layer holds a simple bitmap, size and color format are
    configurable, it works similar to the SketchLayer, however it
    doesn't rerender the image all the time, but simply holds it in a
    CL_Canvas making it a whole lot faster. */
class BitmapLayer
{
  friend class BitmapLayerImpl;
private:
  static BitmapLayer* current_;
public:
  static BitmapLayer* current() { return current_; }
  static void set_current(BitmapLayer* c) { current_ = c; }

  BitmapLayer(CL_Surface surface);
  BitmapLayer(CL_PixelBuffer buffer);
  BitmapLayer(int width, int height);
  
  void add_stroke(const Stroke&);

  std::vector<Stroke> get_strokes();

  CL_Surface get_background_surface();

  void set_pixeldata(CL_PixelBuffer buffer);
  CL_PixelBuffer get_pixeldata() const;
  CL_Canvas*     get_canvas() const;
  
  bool is_null() const { return !impl.get(); }
  ObjMapObject to_object();

private:
  SharedPtr<BitmapLayerImpl> impl;
};

#endif

/* EOF */
