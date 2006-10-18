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
#include "bitmap_layer.hxx"
#include "sketch_stroke_tool.hxx"
#include "sprite_stroke_drawer.hxx"
#include "marker_stroke_drawer.hxx"
#include "stroke.hxx"
#include "stroke_drawer.hxx"
#include "drawer_properties.hxx"
#include "flexlay.hxx"

class SketchStrokeToolImpl : public ToolImpl
{
public:
  bool drawing;
  Stroke   stroke;
  StrokeDrawer   drawer;
  
  SketchStrokeToolImpl()
    : drawing(false)
  {
    drawer = SpriteStrokeDrawer().to_drawer();
    //drawer = MarkerStrokeDrawer().to_drawer();
  }

  void draw() 
  {
    if (drawing)
      {
        // FIXME: This translation is a bit ugly, layer position should be handled somewhat different
        CL_Display::push_modelview();
        CL_Display::add_translate(BitmapLayer::current()->to_object().get_pos().x,
                                  BitmapLayer::current()->to_object().get_pos().y);
        stroke.draw(0);
        CL_Display::pop_modelview();
      }
    else
      {
        EditorMapComponent* parent = EditorMapComponent::current();
        CL_Pointf p = parent->screen2world(CL_Point(CL_Mouse::get_x() - parent->get_screen_x(), 
                                                    CL_Mouse::get_y() - parent->get_screen_y()));
        CL_Sprite s = DrawerProperties::current()->get_brush().get_sprite();
        s.set_color(DrawerProperties::current()->get_color());
        // FIXME: when using mouse 1.0, when tablet .5f
        s.set_scale(DrawerProperties::current()->get_size()*0.5f, DrawerProperties::current()->get_size()*0.5f);
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

        BitmapLayer::current()->add_stroke(stroke);
      }    
  }

  void on_mouse_down(const CL_InputEvent& event) {
    if (event.id == CL_MOUSE_LEFT)
      {
        drawing = true;
        EditorMapComponent* parent = EditorMapComponent::current();
        parent->capture_mouse();
        stroke = Stroke();
        stroke.set_drawer(drawer.clone());
        add_dab(event);
      }
  }

  void add_dab(const CL_InputEvent& event)
  {
    EditorMapComponent* parent = EditorMapComponent::current();
    CL_Pointf p = parent->screen2world(event.mouse_pos);    
    
    // FIXME: This is ugly, events relative to the layer should be handled somewhat differently
    Dab dab(p.x - BitmapLayer::current()->to_object().get_pos().x,
            p.y - BitmapLayer::current()->to_object().get_pos().y);

    // FIXME: Make tablet configurable
    if (CL_Display::get_current_window()->get_ic()->get_mouse_count() >= 4)
      {
        CL_InputDevice tablet = CL_Display::get_current_window()->get_ic()->get_mouse(5);

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

Tool
SketchStrokeTool::to_tool()
{
  return Tool(impl);
}

StrokeDrawer
SketchStrokeTool::get_drawer()
{
  return impl->drawer;
}

/* EOF */
