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

#include <ClanLib/Core/Math/point.h>
#include <ClanLib/Core/Math/rect.h>
#include "tool_impl.hxx"
#include "editor_map_component.hxx"
#include "workspace.hxx"
#include "workspace_move_tool.hxx"

class WorkspaceMoveToolImpl : public ToolImpl
{
public:
  Workspace workspace;

  bool scrolling;
  CL_Point click_pos;

  /** Position of the center */
  CL_Pointf old_trans_offset;
  
  virtual void draw() {}

  void on_mouse_up  (const CL_InputEvent& event);
  void on_mouse_down(const CL_InputEvent& event);
  void on_mouse_move(const CL_InputEvent& event);
  void update(const CL_InputEvent& event);
};

void
WorkspaceMoveToolImpl::on_mouse_down(const CL_InputEvent& event)
{
  scrolling = true;
  old_trans_offset = workspace.get_gc_state().get_pos();
  click_pos = event.mouse_pos;
  EditorMapComponent::current()->capture_mouse();
}

void
WorkspaceMoveToolImpl::on_mouse_up(const CL_InputEvent& event)
{
  scrolling = false;
  update(event);
  old_trans_offset = workspace.get_gc_state().get_pos();
  EditorMapComponent::current()->release_mouse();
}

void
WorkspaceMoveToolImpl::on_mouse_move(const CL_InputEvent& event)
{
  if (scrolling)
    {
      update(event);
    } 
}

void
WorkspaceMoveToolImpl::update(const CL_InputEvent& event)
{
  GraphicContextState& gc_state = workspace.get_gc_state();

  float sa = sin(-gc_state.get_rotation()/180.0f*M_PI);
  float ca = cos(-gc_state.get_rotation()/180.0f*M_PI);

  float dx = ca * (click_pos.x - event.mouse_pos.x) - sa * (click_pos.y - event.mouse_pos.y);
  float dy = sa * (click_pos.x - event.mouse_pos.x) + ca * (click_pos.y - event.mouse_pos.y);

  gc_state.set_pos(CL_Pointf(old_trans_offset.x
                             + dx / workspace.get_gc_state().get_zoom(),
                             old_trans_offset.y
                             + dy / workspace.get_gc_state().get_zoom()));
}

WorkspaceMoveTool::WorkspaceMoveTool(const Workspace& workspace_)
  : impl(new WorkspaceMoveToolImpl())
{
  impl->workspace = workspace_;
  impl->scrolling = false;
  impl->click_pos = CL_Point(0, 0);
  impl->old_trans_offset = CL_Pointf(0,0);
}

Tool
WorkspaceMoveTool::to_tool()
{
  return Tool(impl);
}

/* EOF */
