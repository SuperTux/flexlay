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

#ifndef HEADER_SKETCH_LAYER_HXX
#define HEADER_SKETCH_LAYER_HXX

#include <vector>
#include <ClanLib/Core/Math/point.h>
#include <ClanLib/Display/color.h>
#include "layer.hxx"

class SketchLayerImpl;

class Stroke
{
public:
  typedef std::vector<CL_Pointf> Points;
  Points points;

  CL_Color color;
  
public:
  Stroke() {
    color = CL_Color(255, 255, 255, 255);
  }

  void set_color(const CL_Color& color_) {
    color = color_;
  }

  void add_point(float x, float y) {
    points.push_back(CL_Pointf(x, y));
  }
};

/** Simple drawing layer to add sketches and stuff above a regular
    level */
class SketchLayer
{
private:
  static SketchLayer* current_;
public:
  static SketchLayer* current() { return current_; }

  SketchLayer();
  
  void add_stroke(const Stroke&);

  bool is_null() const { return !impl.get(); }
  Layer to_layer();

private:
  SharedPtr<SketchLayerImpl> impl;  
};

#endif

/* EOF */
