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
#include <ClanLib/Display/input_event.h>
#include <ClanLib/Display/keys.h>
#include <ClanLib/Display/mouse.h>
#include <ClanLib/Display/display.h>
#include <ClanLib/Display/input_context.h>
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
      {
        stroke.draw(0);
      }
    else
      {
        EditorMapComponent* parent = EditorMapComponent::current();
        CL_Pointf p = parent->screen2world(CL_Point(CL_Mouse::get_x() - parent->get_screen_x(), 
                                                    CL_Mouse::get_y() - parent->get_screen_y()));
        CL_Sprite s = sprite_drawer.get_sprite();
        s.set_color(sprite_drawer.get_color());
        s.set_scale(sprite_drawer.get_size(), sprite_drawer.get_size());
        s.set_alpha(0.5);
        s.draw(p.x, p.y);
      }
  }

  void on_mouse_up  (const CL_InputEvent& event) 
  {
    if (event.id == CL_MOUSE_LEFT && drawing)
      {
        drawing = false;
        EditorMapComponent* parent = EditorMapComponent::current();
        parent->release_mouse();
        
        add_dab(event);

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
        add_dab(event);
      }
  }

  void add_dab(const CL_InputEvent& event)
  {
    EditorMapComponent* parent = EditorMapComponent::current();
    CL_Pointf p = parent->screen2world(event.mouse_pos);    

    Dab dab(p.x, p.y);

    if (CL_Display::get_current_window()->get_ic()->get_mouse_count() >= 3)
      {
        CL_InputDevice tablet = CL_Display::get_current_window()->get_ic()->get_mouse(3);

        if (0)
          {
            std::cout << "Mouse Count: " << CL_Display::get_current_window()->get_ic()->get_mouse_count() << std::endl;
            std::cout << tablet.get_name() << ": ";
            for(int i = 0; i < tablet.get_axis_count(); ++i)
              std::cout << tablet.get_axis(i) << " ";
            std::cout << std::endl;
          }

        dab.pressure = tablet.get_axis(2);
        dab.tilt.x   = tablet.get_axis(3);
        dab.tilt.y   = tablet.get_axis(4);
      }

    //std::cout << dab.pressure << " " << dab.tilt.x << " " << dab.tilt.y << std::endl;

    if (dab.pressure == 0) // most likly we are using the mouse
      dab.pressure = 1.0f;

    stroke.add_dab(dab);
  }

  void on_mouse_move(const CL_InputEvent& event) 
  {
    if (drawing)
      {
        add_dab(event);
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
