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
#include <ClanLib/Display/display_window.h>
#include "flexlay.hxx"
#include "layer_impl.hxx"
#include "sketch_layer.hxx"
#include "math.hxx"

SketchLayer* SketchLayer::current_ = 0;

class SketchLayerImpl : public LayerImpl
{
public:
  typedef std::vector<Stroke> Strokes;
  Strokes strokes;

  SketchLayerImpl() 
  {
    
  }
  
  void draw(EditorMapComponent* parent) 
  {
    for(Strokes::iterator i = strokes.begin(); i != strokes.end(); ++i)
      {
        i->draw();
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
  impl->strokes.push_back(stroke);
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
