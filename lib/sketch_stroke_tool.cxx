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
#include <ClanLib/Display/input_event.h>
#include <ClanLib/Display/keys.h>
#include <ClanLib/Display/display.h>
#include "editor_map_component.hxx"
#include "tool.hxx"
#include "sketch_layer.hxx"
#include "sketch_stroke_tool.hxx"
#include "sprite_stroke_drawer.hxx"
#include "stroke.hxx"
#include "stroke_drawer.hxx"
#include "flexlay.hxx"

class SketchStrokeToolImpl : public ToolImpl
{
public:
  bool drawing;
  Stroke   stroke;
  SpriteStrokeDrawer sprite_drawer;

  SketchStrokeToolImpl()
    : drawing(false)
  {
    sprite_drawer.set_sprite(CL_Sprite("brush", &(Flexlay::current()->resources)));
    sprite_drawer.set_color(CL_Color(255, 255, 255, 255));
    sprite_drawer.set_size(1.0f);  
  }

  void draw() 
  {
    if (drawing)
      stroke.draw();
  }

  void on_mouse_up  (const CL_InputEvent& event) 
  {
    if (event.id == CL_MOUSE_LEFT && drawing)
      {
        drawing = false;
        EditorMapComponent* parent = EditorMapComponent::current();
        parent->release_mouse();
        
        CL_Pointf p = parent->screen2world(event.mouse_pos);
        stroke.add_dab(Dab(p.x, p.y));

        SketchLayer::current()->add_stroke(stroke);
      }    
  }

  void on_mouse_down(const CL_InputEvent& event) {
    if (event.id == CL_MOUSE_LEFT)
      {
        drawing = true;
        EditorMapComponent* parent = EditorMapComponent::current();
        parent->capture_mouse();
        stroke = Stroke();
        stroke.set_drawer(sprite_drawer.to_drawer().clone());
        CL_Pointf p = parent->screen2world(event.mouse_pos);
        stroke.add_dab(Dab(p.x, p.y));
      }
  }

  void on_mouse_move(const CL_InputEvent& event) 
  {
    if (drawing)
      {
        EditorMapComponent* parent = EditorMapComponent::current();
        CL_Pointf p = parent->screen2world(event.mouse_pos);
        stroke.add_dab(Dab(p.x, p.y));
      }
  }
};

SketchStrokeTool::SketchStrokeTool()
  : impl(new SketchStrokeToolImpl()) 
{
}

void 
SketchStrokeTool::set_color(const CL_Color& color_)
{
  impl->sprite_drawer.set_color(color_);
}

void
SketchStrokeTool::set_size(float size_)
{
  impl->sprite_drawer.set_size(size_);
}

Tool
SketchStrokeTool::to_tool()
{
  return Tool(impl);
}

/* EOF */
