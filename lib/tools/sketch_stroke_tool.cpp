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

#include "sketch_stroke_tool.hpp"

#include "bitmap_layer.hpp"
#include "display.hpp"
#include "drawer_properties.hpp"
#include "graphic_context.hpp"
#include "gui/editor_map_component.hpp"
#include "input_event.hpp"
#include "sprite_stroke_drawer.hpp"

class SketchStrokeToolImpl : public ToolImpl
{
public:
  bool drawing;
  Stroke stroke;
  StrokeDrawer drawer;

  SketchStrokeToolImpl()
    : drawing(false)
  {
    drawer = SpriteStrokeDrawer().to_drawer();
    //drawer = MarkerStrokeDrawer().to_drawer();
  }

  void draw(GraphicContext& gc)
  {
#ifdef GRUMBEL
    if (drawing)
    {
      // FIXME: This translation is a bit ugly, layer position should be handled somewhat different
      gc.push_modelview();
      gc.add_translate(BitmapLayer::current()->to_object().get_pos().x,
                       BitmapLayer::current()->to_object().get_pos().y);
      stroke.draw(0);
      gc.pop_modelview();
    }
    else
    {
      EditorMapComponent* parent = EditorMapComponent::current();
      Pointf p = parent->screen2world(Point(CL_Mouse::get_x() - parent->get_screen_x(),
                                            CL_Mouse::get_y() - parent->get_screen_y()));
      Sprite s = DrawerProperties::current()->get_brush().get_sprite();
      s.set_color(DrawerProperties::current()->get_color().to_cl());
      // FIXME: when using mouse 1.0, when tablet .5f
      s.set_scale(DrawerProperties::current()->get_size()*0.5f, DrawerProperties::current()->get_size()*0.5f);
      s.set_alpha(0.5);
      s.draw(p.x, p.y);
    }
#endif
  }

  void on_mouse_up  (const InputEvent& event)
  {
#ifdef GRUMBEL
    if (event.id == InputEvent::MOUSE_LEFT && drawing)
    {
      drawing = false;
      EditorMapComponent* parent = EditorMapComponent::current();
      parent->release_mouse();

      add_dab(event);

      BitmapLayer::current()->add_stroke(stroke);
    }
#endif
  }

  void on_mouse_down(const InputEvent& event)
  {
#ifdef GRUMBEL
    if (event.id == InputEvent::MOUSE_LEFT)
    {
      drawing = true;
      EditorMapComponent* parent = EditorMapComponent::current();
      parent->capture_mouse();
      stroke = Stroke();
      stroke.set_drawer(drawer.clone());
      add_dab(event);
    }
#endif
  }

  void add_dab(const InputEvent& event)
  {
#ifdef GRUMBEL
    EditorMapComponent* parent = EditorMapComponent::current();
    Pointf p = parent->screen2world(event.mouse_pos);

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
#endif
  }

  void on_mouse_move(const InputEvent& event)
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
