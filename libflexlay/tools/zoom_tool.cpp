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

#include "zoom_tool.hpp"

#include "color.hpp"
#include "graphic_context.hpp"
#include "gui/editor_map_component.hpp"
#include "input_event.hpp"
#include "tool_impl.hpp"

class ZoomToolImpl : public ToolImpl
{
public:
  enum { CREATE_ZOOM_RECT, NONE } state;

  Rectf zoom_rect;

  void draw(GraphicContext& gc);

  void on_mouse_up(const InputEvent& event);
  void on_mouse_down(const InputEvent& event);
  void on_mouse_move(const InputEvent& event);
};

ZoomTool::ZoomTool()
  : impl(new ZoomToolImpl())
{
  impl->state = ZoomToolImpl::NONE;
}

ZoomTool::~ZoomTool()
{
}

void
ZoomToolImpl::draw(GraphicContext& gc)
{
  switch (state)
  {
    case CREATE_ZOOM_RECT:
      {
        Rectf tmp(zoom_rect);
        tmp.normalize();
        gc.fill_rect(tmp, Color(255, 255, 0, 50));
        gc.draw_rect(tmp, Color(255, 255, 0, 200));
      }
    case NONE:
      break;
  }
}

void
ZoomToolImpl::on_mouse_up(const InputEvent& event)
{
  EditorMapComponent* parent = EditorMapComponent::current();

  if (event.id != InputEvent::MOUSE_RIGHT)
  {
    switch (state)
    {
      case CREATE_ZOOM_RECT:
        {
          state = NONE;
          parent->release_mouse();

          Pointf pos = parent->screen2world(event.mouse_pos);
          zoom_rect.right  = pos.x;
          zoom_rect.bottom = pos.y;
          zoom_rect.normalize();
          if (zoom_rect.get_width() > 10 && zoom_rect.get_height() > 10)
          {
            parent->zoom_to(zoom_rect);
          }
        }
        break;

      default:
        break;
    }
  }
}

void
ZoomToolImpl::on_mouse_down(const InputEvent& event)
{
  EditorMapComponent* parent = EditorMapComponent::current();

  switch(event.id)
  {
    case InputEvent::MOUSE_RIGHT:
      switch (state)
      {
        case NONE:
          parent->zoom_out(event.mouse_pos);
          parent->zoom_out(event.mouse_pos);
          break;
        default:
          break;
      }
      break;

    default:
      switch (state)
      {
        case NONE:
        {
          state = CREATE_ZOOM_RECT;
          parent->capture_mouse();

          Pointf pos = parent->screen2world(event.mouse_pos);
          zoom_rect.left   = pos.x;
          zoom_rect.top    = pos.y;
          zoom_rect.right  = pos.x;
          zoom_rect.bottom = pos.y;
        }
        break;
        default:
          break;
      }
      break;
  }
}

void
ZoomToolImpl::on_mouse_move(const InputEvent& event)
{
  EditorMapComponent* parent = EditorMapComponent::current();

  switch (state)
  {
    case CREATE_ZOOM_RECT:
    {
      Pointf pos = parent->screen2world(event.mouse_pos);
      zoom_rect.right  = pos.x;
      zoom_rect.bottom = pos.y;
    }
    break;
    default:
      break;
  }
}

Tool
ZoomTool::to_tool()
{
  return Tool(impl);
}

/* EOF */
