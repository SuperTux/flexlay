// Flexlay - A Generic 2D Game Editor
// Copyright (C) 2004 Ingo Ruhnke <grumbel@gmail.com>
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

#include "layer_move_tool.hpp"

#include "display.hpp"
#include "editor_map.hpp"
#include "gui/editor_map_component.hpp"
#include "input_event.hpp"
#include "layer.hpp"
#include "tool_impl.hpp"
#include "workspace.hpp"

class LayerMoveToolImpl : public ToolImpl
{
public:
  bool scrolling;
  Pointf click_pos;

  /** Position of the center */
  Pointf old_trans_offset;
  Layer layer;

  Layer find_closed_layer(const Pointf& pos)
  {
    Layer layer;

    EditorMap parent = EditorMapComponent::current()->get_workspace().get_map();

    for(int i = 0; i < parent.get_layer_count(); ++i)
    {
      if (parent.get_layer(i).get_bounding_rect().is_inside(Point(pos)))
        layer = parent.get_layer(i);
    }

    return layer;
  }

  void draw()
  {
    for(int i = 0; i < EditorMapComponent::current()->get_workspace().get_map().get_layer_count(); ++i)
    {
      Layer layer = EditorMapComponent::current()->get_workspace().get_map().get_layer(i);
      if (layer.has_bounding_rect())
      {
        Rect rect = layer.get_bounding_rect();
        Display::draw_line(rect.left, rect.top, rect.right, rect.bottom,
                              Color(0, 255, 255).to_cl());
        Display::draw_line(rect.left, rect.bottom, rect.right, rect.top,
                              Color(0, 255, 255).to_cl());
      }
    }
  }

  void on_mouse_up  (const InputEvent& event)
  {
    if (!layer.is_null())
    {
      scrolling = false;
      update(event);
      EditorMapComponent::current()->release_mouse();
      layer = Layer();
    }
  }

  void on_mouse_down(const InputEvent& event)
  {
    EditorMapComponent* parent = EditorMapComponent::current();
    Pointf pos = parent->screen2world(event.mouse_pos);

    layer = find_closed_layer(pos);
    if (!layer.is_null())
    {
      scrolling = true;
      old_trans_offset = layer.get_pos();
      click_pos = pos;
      EditorMapComponent::current()->capture_mouse();
    }
  }

  void on_mouse_move(const InputEvent& event)
  {
    if (!layer.is_null())
    {
      if (scrolling)
      {
        update(event);
      }
    }
  }

  void update(const InputEvent& event)
  {
    if (!layer.is_null())
    {
      EditorMapComponent* parent = EditorMapComponent::current();
      Pointf pos = parent->screen2world(event.mouse_pos);
      layer.set_pos(old_trans_offset + (pos - click_pos));
    }
  }
};

LayerMoveTool::LayerMoveTool()
  : impl(new LayerMoveToolImpl())
{
  impl->scrolling = false;
  impl->click_pos = Point(0, 0);
  impl->old_trans_offset = Pointf(0,0);
}

Tool
LayerMoveTool::to_tool()
{
  return Tool(impl);
}

/* EOF */

