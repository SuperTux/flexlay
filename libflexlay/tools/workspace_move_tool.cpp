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

#include "workspace_move_tool.hpp"

#include "gui/editor_map_component.hpp"
#include "input_event.hpp"
#include "tool_impl.hpp"

class WorkspaceMoveToolImpl : public ToolImpl
{
public:
  bool scrolling;
  Point click_pos;

  /** Position of the center */
  Pointf old_trans_offset;

  virtual void draw(GraphicContext& gc) {}

  void on_mouse_up  (const InputEvent& event);
  void on_mouse_down(const InputEvent& event);
  void on_mouse_move(const InputEvent& event);
  void update(const InputEvent& event);
};

void
WorkspaceMoveToolImpl::on_mouse_down(const InputEvent& event)
{
  scrolling = true;
  old_trans_offset = EditorMapComponent::current()->get_gc_state().get_pos();
  click_pos = event.mouse_pos;
  EditorMapComponent::current()->capture_mouse();
}

void
WorkspaceMoveToolImpl::on_mouse_up(const InputEvent& event)
{
  scrolling = false;
  update(event);
  old_trans_offset = EditorMapComponent::current()->get_gc_state().get_pos();
  EditorMapComponent::current()->release_mouse();
}

void
WorkspaceMoveToolImpl::on_mouse_move(const InputEvent& event)
{
  if (scrolling)
  {
    update(event);
  }
}

void
WorkspaceMoveToolImpl::update(const InputEvent& event)
{
  GraphicContextState& gc_state = EditorMapComponent::current()->get_gc_state();

  float sa = sin(-gc_state.get_rotation()/180.0f*M_PI);
  float ca = cos(-gc_state.get_rotation()/180.0f*M_PI);

  float dx = ca * (click_pos.x - event.mouse_pos.x) - sa * (click_pos.y - event.mouse_pos.y);
  float dy = sa * (click_pos.x - event.mouse_pos.x) + ca * (click_pos.y - event.mouse_pos.y);

  gc_state.set_pos(Pointf(old_trans_offset.x
                             + dx / EditorMapComponent::current()->get_gc_state().get_zoom(),
                             old_trans_offset.y
                             + dy / EditorMapComponent::current()->get_gc_state().get_zoom()));
}

WorkspaceMoveTool::WorkspaceMoveTool()
  : impl(new WorkspaceMoveToolImpl())
{
  impl->scrolling = false;
  impl->click_pos = Point(0, 0);
  impl->old_trans_offset = Pointf(0,0);
}

Tool
WorkspaceMoveTool::to_tool()
{
  return Tool(impl);
}

/* EOF */
