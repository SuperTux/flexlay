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

#include "gui/editor_map_component.hpp"
#include "input_event.hpp"
#include "tool_impl.hpp"
#include "zoom2_tool.hpp"

class Zoom2ToolImpl : public ToolImpl
{
public:
  bool active;
  Point click_pos;
  float old_zoom;
  void draw(GraphicContext& gc);

  void on_mouse_up  (const InputEvent& event);
  void on_mouse_down(const InputEvent& event);
  void on_mouse_move(const InputEvent& event);
};

Zoom2Tool::Zoom2Tool()
  : impl(new Zoom2ToolImpl())
{
  impl->active = false;
}

Zoom2Tool::~Zoom2Tool()
{
}

void
Zoom2ToolImpl::draw(GraphicContext& gc)
{
}

void
Zoom2ToolImpl::on_mouse_up  (const InputEvent& event)
{
  active = false;
}

void
Zoom2ToolImpl::on_mouse_down(const InputEvent& event)
{
  active = true;
  click_pos = event.mouse_pos;

  GraphicContextState& gc = EditorMapComponent::current()->get_gc_state();
  old_zoom = gc.get_zoom();
}

void
Zoom2ToolImpl::on_mouse_move(const InputEvent& event)
{
  if (active)
  {
    GraphicContextState& gc = EditorMapComponent::current()->get_gc_state();

    Point zoom_pos(gc.get_width()/2,
                      gc.get_height()/2);

    float factor = (event.mouse_pos.y - click_pos.y) / 20.0f;
    if (factor > 0)
      gc.set_zoom(old_zoom * pow(1.25f, factor), zoom_pos);
    else if (factor < 0)
      gc.set_zoom(old_zoom / pow(1.25f, -factor), zoom_pos);
    else
      gc.set_zoom(old_zoom, zoom_pos);
  }
}

Tool
Zoom2Tool::to_tool()
{
  return Tool(impl);
}

/* EOF */
