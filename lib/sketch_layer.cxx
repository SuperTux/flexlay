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
#include "layer_impl.hxx"
#include "sketch_layer.hxx"

SketchLayer* SketchLayer::current_ = 0;

class SketchLayerImpl : public LayerImpl
{
public:
  typedef std::vector<Stroke> Strokes;
  Strokes strokes;

  void draw(EditorMapComponent* parent) 
  {
    glLineWidth(3.0);
    for(Strokes::iterator i = strokes.begin(); i != strokes.end(); ++i)
      {
        if (i->points.size() >= 2)
          {
            Stroke::Points::iterator last = i->points.begin();
            for(Stroke::Points::iterator j = i->points.begin()+1; j != i->points.end(); ++j)
              {
                //std::cout << *last << " -> " << *j << std::endl;
                CL_Display::draw_line(*last, *j, i->color);
                last = j;
              }
          }
      }
    glLineWidth(1.0);
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
  impl->strokes.push_back(stroke);
}

Layer
SketchLayer::to_layer()
{
   return Layer(impl);
}


/* EOF */
