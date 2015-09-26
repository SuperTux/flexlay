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

#ifndef HEADER_FLEXLAY_SKETCH_LAYER_HPP
#define HEADER_FLEXLAY_SKETCH_LAYER_HPP

#include <vector>

#include "surface.hpp"
#include "layer.hpp"
#include "stroke.hpp"

class SketchLayerImpl;

/** A drawing layer that holds strokes and renders them more or less
    efficently to the screen, for larger number of strokes this has
    serious performance impact, use BitmapLayer instead */
class SketchLayer
{
private:
  static SketchLayer* current_;
public:
  static SketchLayer* current() { return current_; }
  static void set_current(SketchLayer* c) { current_ = c; }

  SketchLayer();

  void add_stroke(const Stroke&);

  std::vector<Stroke> get_strokes();

  Surface get_background_surface();

  bool is_null() const { return !impl.get(); }
  Layer to_layer();

private:
  std::shared_ptr<SketchLayerImpl> impl;
};

#endif

/* EOF */
