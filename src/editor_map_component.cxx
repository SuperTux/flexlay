//  $Id$
//
//  Flexlay - A Generic 2D Game Editor
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
#include "editor.hxx"
#include "workspace.hxx"
#include "scrollbar.hxx"
#include "editor_map_component.hxx"

EditorMapComponent* EditorMapComponent::current_ = 0; 

EditorMapComponent::EditorMapComponent(const CL_Rect& rect, CL_Component* parent)
  : CL_Component(rect, parent),
    workspace(rect.get_width(), rect.get_height())
{
  current_ = this;

  scrollbar_v = new Scrollbar(CL_Rect(CL_Point(rect.get_width() - 14, 2), 
                                      CL_Size(12, rect.get_height() - 4 - 14)),
                              Scrollbar::VERTICAL,
                              this);

  scrollbar_h = new Scrollbar(CL_Rect(CL_Point(2, rect.get_height() - 14), 
                                      CL_Size(rect.get_width() - 4 - 14, 12)),
                              Scrollbar::HORIZONTAL,
                              this);

  slots.connect(scrollbar_h->sig_scrollbar_move(), this, &EditorMapComponent::move_to_x);
  slots.connect(scrollbar_v->sig_scrollbar_move(), this, &EditorMapComponent::move_to_y);

  slots.connect(sig_paint(),      this, &EditorMapComponent::draw);
  slots.connect(sig_mouse_up(),   this, &EditorMapComponent::mouse_up);
  slots.connect(sig_mouse_down(), this, &EditorMapComponent::mouse_down);
  slots.connect(sig_mouse_move(), this, &EditorMapComponent::mouse_move);
}

EditorMapComponent::~EditorMapComponent()
{
  std::cout << "~EditorMapComponent()" << std::endl;
}

Workspace
EditorMapComponent::get_workspace() const
{
  return workspace;
}

void
EditorMapComponent::set_workspace(Workspace m)
{
  workspace = m;
}

void
EditorMapComponent::mouse_up(const CL_InputEvent& event)
{
  workspace.mouse_up(event);
}

void
EditorMapComponent::mouse_move(const CL_InputEvent& event)
{
  workspace.mouse_move(event);
}

void
EditorMapComponent::mouse_down(const CL_InputEvent& event)
{
  workspace.mouse_down(event);
}
  
void
EditorMapComponent::draw ()
{
  // Update scrollbars (FIXME: move me to function)
  scrollbar_v->set_range(0, workspace.get_map().get_bounding_rect().get_height());
  scrollbar_v->set_pagesize(get_height()/workspace.get_gc_state().get_zoom());
  scrollbar_v->set_pos(workspace.get_gc_state().get_pos().y);

  scrollbar_h->set_range(0, workspace.get_map().get_bounding_rect().get_width());
  scrollbar_h->set_pagesize(get_width()/workspace.get_gc_state().get_zoom());
  scrollbar_h->set_pos(workspace.get_gc_state().get_pos().x);

  workspace.draw();
}

CL_Point
EditorMapComponent::screen2world(const CL_Point& pos)
{
  CL_Pointf p = workspace.get_gc_state().screen2world(pos);
  return CL_Point((int)p.x, (int)p.y);
}

void
EditorMapComponent::set_zoom(float z)
{
  workspace.get_gc_state().set_zoom(z);
}

void
EditorMapComponent::zoom_out(CL_Point pos)
{
  workspace.get_gc_state().set_zoom(CL_Pointf(pos.x, pos.y), workspace.get_gc_state().get_zoom()/1.25f);
}

void
EditorMapComponent::zoom_in(CL_Point pos)
{
  workspace.get_gc_state().set_zoom(CL_Pointf(pos.x, pos.y), workspace.get_gc_state().get_zoom()*1.25f);
}

void
EditorMapComponent::zoom_to(CL_Rect rect)
{
  workspace.get_gc_state().zoom_to(rect);
}

CL_Rect
EditorMapComponent::get_clip_rect()
{
  return workspace.get_gc_state().get_clip_rect();
}

void
EditorMapComponent::move_to(int x, int y)
{
  workspace.get_gc_state().set_pos(CL_Pointf(x, y));
}

void
EditorMapComponent::move_to_x(float x)
{
  workspace.get_gc_state().set_pos(CL_Pointf(x, workspace.get_gc_state().get_pos().y));
}

void
EditorMapComponent::move_to_y(float y)
{
  workspace.get_gc_state().set_pos(CL_Pointf(workspace.get_gc_state().get_pos().x, y));
}

/* EOF */
