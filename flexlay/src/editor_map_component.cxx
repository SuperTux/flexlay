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
#include <ClanLib/Core/core_iostream.h>
#include <ClanLib/Display/display.h>
#include <ClanLib/Display/keys.h>
#include "tileset.hxx"
#include "editor_names.hxx"
#include "editor_map.hxx"
#include "tool_manager.hxx"
#include "tilemap_tool.hxx"
#include "editor.hxx"
#include "workspace.hxx"
#include "editor_map_component.hxx"

EditorMapComponent* EditorMapComponent::current_ = 0; 

EditorMapComponent::EditorMapComponent(const CL_Rect& rect, CL_Component* parent)
  : CL_Component(rect, parent)
{
  current_ = this;

  slots.connect(sig_paint(),      this, &EditorMapComponent::draw);
  slots.connect(sig_mouse_up(),   this, &EditorMapComponent::mouse_up);
  slots.connect(sig_mouse_down(), this, &EditorMapComponent::mouse_down);
  slots.connect(sig_mouse_move(), this, &EditorMapComponent::mouse_move);

  workspace = new Workspace(get_width(), get_height());
}

EditorMapComponent::~EditorMapComponent()
{
  delete workspace;
}

EditorMap*
EditorMapComponent::get_map() const
{
  return workspace->get_current_item()->editor_map; 
}

void
EditorMapComponent::set_map(EditorMap* m)
{
  workspace->get_current_item()->editor_map = m; 
}

void
EditorMapComponent::mouse_up(const CL_InputEvent& event)
{
  workspace->mouse_up(event);
}

void
EditorMapComponent::mouse_move(const CL_InputEvent& event)
{
  workspace->mouse_move(event);
}

void
EditorMapComponent::mouse_down(const CL_InputEvent& event)
{
  workspace->mouse_down(event);
}
  
void
EditorMapComponent::draw ()
{
  workspace->draw();
}

CL_Point
EditorMapComponent::screen2world(const CL_Point& pos)
{
  CL_Pointf p = workspace->gc_state.screen2world(pos);
  return CL_Point((int)p.x, (int)p.y);
}

void
EditorMapComponent::set_zoom(float z)
{
  workspace->gc_state.set_zoom(z);
}

void
EditorMapComponent::zoom_out(CL_Point pos)
{
  workspace->gc_state.set_zoom(pos, workspace->gc_state.get_zoom()/1.25f);
}

void
EditorMapComponent::zoom_in(CL_Point pos)
{
  workspace->gc_state.set_zoom(pos, workspace->gc_state.get_zoom()*1.25f);
}

void
EditorMapComponent::zoom_to(CL_Rect rect)
{
  workspace->gc_state.zoom_to(rect);
}

CL_Rect
EditorMapComponent::get_clip_rect()
{
  return workspace->gc_state.get_clip_rect();
}

void
EditorMapComponent::move_to(int x, int y)
{
  workspace->gc_state.set_pos(CL_Pointf(x, y));
}

/* EOF */
