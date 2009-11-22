//  $Id$
//
//  Flexlay - A Generic 2D Game Editor
//  Copyright (C) 2002 Ingo Ruhnke <grumbel@gmx.de>
//
//  This program is free software: you can redistribute it and/or modify
//  it under the terms of the GNU General Public License as published by
//  the Free Software Foundation, either version 3 of the License, or
//  (at your option) any later version.
//  
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//  GNU General Public License for more details.
//  
//  You should have received a copy of the GNU General Public License
//  along with this program.  If not, see <http://www.gnu.org/licenses/>.

#include <iostream>
#include <ClanLib/Core/core_iostream.h>
#include <ClanLib/Display/keyboard.h>
#include <ClanLib/Display/mouse.h>
#include <ClanLib/Display/display.h>
#include <ClanLib/Display/display_iostream.h>
#include <ClanLib/Display/keys.h>
#include "tileset.hpp"
#include "editor_names.hpp"
#include "editor_map.hpp"
#include "workspace.hpp"
#include "scrollbar.hpp"
#include "editor_map_component.hpp"

EditorMapComponent* EditorMapComponent::current_ = 0; 

class EditorMapComponentImpl
{
public:
  EditorMapComponent* parent;
  GraphicContextState gc_state;
  Scrollbar* scrollbar_h;
  Scrollbar* scrollbar_v;
  CL_SlotContainer slots;
  Workspace workspace;
  CL_Signal_v2<int, int> key_bindings[256];

  EditorMapComponentImpl()
    :workspace(true) 
  {}

  void draw();
  void mouse_up  (const CL_InputEvent& event);
  void mouse_down(const CL_InputEvent& event);
  void mouse_move(const CL_InputEvent& event);
  void on_key_up(const CL_InputEvent& event);
  void on_key_down(const CL_InputEvent& event);
  void on_resize(int old_w, int old_h);
};

EditorMapComponent::EditorMapComponent(const CL_Rect& rect, CL_Component* parent)
  : CL_Component(rect, parent),
    impl(new EditorMapComponentImpl())
{
  impl->parent = this;
  impl->gc_state  = GraphicContextState(rect.get_width(), rect.get_height());

  current_ = this;

  impl->scrollbar_v = new Scrollbar(CL_Rect(CL_Point(rect.get_width() - 14, 2) + CL_Point(rect.left, rect.top), 
                                            CL_Size(12, rect.get_height() - 4 - 14)),
                                    Scrollbar::VERTICAL,
                                    parent);

  impl->scrollbar_h = new Scrollbar(CL_Rect(CL_Point(2, rect.get_height() - 14) + CL_Point(rect.left, rect.top), 
                                            CL_Size(rect.get_width() - 4 - 14, 12)),
                                    Scrollbar::HORIZONTAL,
                                    parent);

  impl->slots.connect(impl->scrollbar_h->sig_scrollbar_move(), this, &EditorMapComponent::move_to_x);
  impl->slots.connect(impl->scrollbar_v->sig_scrollbar_move(), this, &EditorMapComponent::move_to_y);

  impl->slots.connect(sig_paint(),      impl.get(), &EditorMapComponentImpl::draw);
  impl->slots.connect(sig_mouse_up(),   impl.get(), &EditorMapComponentImpl::mouse_up);
  impl->slots.connect(sig_mouse_down(), impl.get(), &EditorMapComponentImpl::mouse_down);
  impl->slots.connect(sig_mouse_move(), impl.get(), &EditorMapComponentImpl::mouse_move);
  impl->slots.connect(sig_key_down(),   impl.get(), &EditorMapComponentImpl::on_key_down);
  impl->slots.connect(sig_key_up(),     impl.get(), &EditorMapComponentImpl::on_key_up);
  impl->slots.connect(sig_resize(),     impl.get(), &EditorMapComponentImpl::on_resize);
}

EditorMapComponent::~EditorMapComponent()
{
  std::cout << "~EditorMapComponent()" << std::endl;
}

Workspace
EditorMapComponent::get_workspace() const
{
  return impl->workspace;
}

void
EditorMapComponent::set_workspace(Workspace m)
{
  impl->workspace = m;
}

void
EditorMapComponentImpl::on_key_down(const CL_InputEvent& event)
{
  if (event.id >= 0 && event.id < 256)
    { 
      CL_Rect rect = parent->get_position();
      key_bindings[event.id](CL_Mouse::get_x() - rect.left,
                             CL_Mouse::get_y() - rect.top);
    }

  if (event.repeat_count == 0)
    {
      CL_Rect rect = parent->get_position();
      CL_InputEvent ev2 = event;
      ev2.mouse_pos = CL_Point(CL_Mouse::get_x() - rect.left,
                               CL_Mouse::get_y() - rect.top);
      workspace.key_down(ev2);
    }
}

void
EditorMapComponentImpl::on_key_up(const CL_InputEvent& event)
{
  CL_Rect rect = parent->get_position();
  CL_InputEvent ev2 = event;
  ev2.mouse_pos = CL_Point(CL_Mouse::get_x() - rect.left,
                           CL_Mouse::get_y() - rect.top);
  workspace.key_up(ev2);
}

void
EditorMapComponentImpl::mouse_up(const CL_InputEvent& event)
{
  workspace.mouse_up(event);
}

void
EditorMapComponentImpl::mouse_move(const CL_InputEvent& event)
{
  workspace.mouse_move(event);
}

void
EditorMapComponentImpl::mouse_down(const CL_InputEvent& event)
{
  workspace.mouse_down(event);
}
  
void
EditorMapComponentImpl::draw ()
{
  if (workspace.get_map().is_null()) return;

  CL_Display::push_cliprect(parent->get_screen_rect());

  CL_Display::push_translate(parent->get_screen_x(), parent->get_screen_y());

  // Update scrollbars (FIXME: move me to function)
  scrollbar_v->set_range(0, workspace.get_map().get_bounding_rect().get_height());
  scrollbar_v->set_pagesize(parent->get_height()/gc_state.get_zoom());
  scrollbar_v->set_pos(gc_state.get_pos().y);

  scrollbar_h->set_range(0, workspace.get_map().get_bounding_rect().get_width());
  scrollbar_h->set_pagesize(parent->get_width()/gc_state.get_zoom());
  scrollbar_h->set_pos(gc_state.get_pos().x);

  gc_state.push();
  workspace.draw();
  gc_state.pop();

  CL_Display::pop_modelview();
  CL_Display::pop_cliprect();
}

CL_Pointf
EditorMapComponent::screen2world(const CL_Point& pos)
{
  return impl->gc_state.screen2world(pos);
}

void
EditorMapComponent::set_zoom(float z)
{
  impl->gc_state.set_zoom(z);
}

void
EditorMapComponent::zoom_out(CL_Point pos)
{
  impl->gc_state.set_zoom(CL_Pointf(pos.x, pos.y),
                          impl->gc_state.get_zoom()/1.25f);
}

void
EditorMapComponent::zoom_in(CL_Point pos)
{
  impl->gc_state.set_zoom(CL_Pointf(pos.x, pos.y), 
                          impl->gc_state.get_zoom()*1.25f);
}

void
EditorMapComponent::zoom_to(CL_Rectf rect)
{
  impl->gc_state.zoom_to(rect);
}

CL_Rectf
EditorMapComponent::get_clip_rect()
{
  return impl->gc_state.get_clip_rect();
}

void
EditorMapComponent::move_to(int x, int y)
{
  impl->gc_state.set_pos(CL_Pointf(x, y));
}

void
EditorMapComponent::move_to_x(float x)
{
  impl->gc_state.set_pos(CL_Pointf(x, impl->gc_state.get_pos().y));
}

void
EditorMapComponent::move_to_y(float y)
{
  impl->gc_state.set_pos(CL_Pointf(impl->gc_state.get_pos().x, y));
}

void
EditorMapComponentImpl::on_resize(int old_w, int old_h)
{
  CL_Rect rect = parent->get_screen_rect();

  scrollbar_v->set_position(rect.get_width() - 14 + rect.left,  2 + rect.top);
  scrollbar_v->set_size(12, rect.get_height() - 4 - 14);
  
  scrollbar_h->set_position(2 + rect.left, rect.get_height() - 14 + rect.top);
  scrollbar_h->set_size(rect.get_width() - 4 - 14, 12);

  gc_state.set_size(rect.get_width(), rect.get_height());
}

CL_Signal_v2<int, int>&
EditorMapComponent::sig_on_key(const std::string& str)
{
  int id = CL_Keyboard::get_device().string_to_keyid(str);

  //std::cout << str << " => " << id << std::endl;

  if (id > 0 && id < 256)
    {
      return impl->key_bindings[id];
    }
  else
    {
      std::cout << "EditorMapComponent::sig_on_key: invalid key id: " << id << std::endl;
      return impl->key_bindings[0];
    }
}

GraphicContextState&
EditorMapComponent::get_gc_state()
{
  return impl->gc_state;
}

/* EOF */
