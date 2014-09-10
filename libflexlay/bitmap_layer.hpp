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

#ifndef HEADER_FLEXLAY_BITMAP_LAYER_HPP
#define HEADER_FLEXLAY_BITMAP_LAYER_HPP

#include "objmap_object.hpp"
#include "pixel_buffer.hpp"
#include "stroke.hpp"
#include "surface.hpp"

class BitmapLayerImpl;
class CL_Canvas;

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

  BitmapLayer(Surface surface);
  BitmapLayer(PixelBuffer buffer);
  BitmapLayer(int width, int height);

  void add_stroke(const Stroke&);

  std::vector<Stroke> get_strokes();

  Surface get_background_surface();

  void set_pixeldata(PixelBuffer buffer);
  PixelBuffer get_pixeldata() const;
  CL_Canvas*     get_canvas() const;

  bool is_null() const { return !impl.get(); }
  ObjMapObject to_object();

private:
  std::shared_ptr<BitmapLayerImpl> impl;
};

#endif

/* EOF */
