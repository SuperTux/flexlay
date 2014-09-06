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

#include "editor_map_component.hpp"

#include <QGLWidget>

#include <functional>
#include <iostream>

#include "display.hpp"
#include "editor_map.hpp"
#include "graphic_context.hpp"
#include "input_event.hpp"
#include "editor_map_widget.hpp"

EditorMapComponent* EditorMapComponent::current_ = 0;

class EditorMapComponent;

EditorMapComponent::EditorMapComponent(QWidget* parent) :
  m_editormap_widget(new EditorMapWidget(*this, parent))
{
  current_ = this;

#ifdef GRUMBEL
  impl->parent = this;
  ////m_gc_state  = GraphicContextState(rect.get_width(), rect.get_height());

  impl->scrollbar_h->sig_scrollbar_move().connect(std::bind(&EditorMapComponent::move_to_x, this, std::placeholders::_1));
  impl->scrollbar_v->sig_scrollbar_move().connect(std::bind(&EditorMapComponent::move_to_y, this, std::placeholders::_1));
#endif
}

EditorMapComponent::~EditorMapComponent()
{
  std::cout << "~EditorMapComponent()" << std::endl;
}

Workspace
EditorMapComponent::get_workspace() const
{
  return m_workspace;
}

void
EditorMapComponent::set_workspace(Workspace m)
{
  m_workspace = m;
}

void
EditorMapComponent::capture_mouse()
{
  m_editormap_widget->grabMouse();
}

void
EditorMapComponent::release_mouse()
{
  m_editormap_widget->releaseMouse();
}

#ifdef GRUMBEL
void
EditorMapComponentImpl::on_key_down(const CL_InputEvent& event)
{
  if (event.id >= 0 && event.id < 256)
  {
    Rect rect = parent->get_position();
    key_bindings[event.id](CL_Mouse::get_x() - rect.left,
                           CL_Mouse::get_y() - rect.top);
  }

  if (event.repeat_count == 0)
  {
    Rect rect = parent->get_position();
    CL_InputEvent ev2 = event;
    ev2.mouse_pos = Point(CL_Mouse::get_x() - rect.left,
                          CL_Mouse::get_y() - rect.top).to_cl();
    workspace.key_down(InputEvent(ev2));
  }
}

void
EditorMapComponentImpl::on_key_up(const CL_InputEvent& event)
{
  Rect rect = parent->get_position();
  CL_InputEvent ev2 = event;
  ev2.mouse_pos = Point(CL_Mouse::get_x() - rect.left,
                        CL_Mouse::get_y() - rect.top).to_cl();
  workspace.key_up(InputEvent(ev2));
}

void
EditorMapComponentImpl::draw ()
{
  if (workspace.get_map().is_null()) return;

  Display::push_cliprect(parent->get_screen_rect());

  Display::push_modelview();
  Display::add_translate(parent->get_screen_x(), parent->get_screen_y());

  // Update scrollbars (FIXME: move me to function)
  scrollbar_v->set_range(0, workspace.get_map().get_bounding_rect().get_height());
  scrollbar_v->set_pagesize(parent->get_height()/gc_state.get_zoom());
  scrollbar_v->set_pos(gc_state.get_pos().y);

  scrollbar_h->set_range(0, workspace.get_map().get_bounding_rect().get_width());
  scrollbar_h->set_pagesize(parent->get_width()/gc_state.get_zoom());
  scrollbar_h->set_pos(gc_state.get_pos().x);

  gc_state.push();
  {
    GraphicContext gc(gc_state, CL_Display::get_current_window()->get_gc());
    workspace.draw(gc);
  }
  gc_state.pop();

  Display::pop_modelview();
  Display::pop_cliprect();
}
#endif

Pointf
EditorMapComponent::screen2world(const Point& pos)
{
  return m_gc_state.screen2world(pos);
}

void
EditorMapComponent::set_zoom(float z)
{
#ifdef GRUMBEL
  m_gc_state.set_zoom(z);
#endif
}

void
EditorMapComponent::zoom_out(Point pos)
{
#ifdef GRUMBEL
  m_gc_state.set_zoom(Pointf(pos.x, pos.y),
                          m_gc_state.get_zoom()/1.25f);
#endif
}

void
EditorMapComponent::zoom_in(Point pos)
{
#ifdef GRUMBEL
  m_gc_state.set_zoom(Pointf(pos.x, pos.y),
                          m_gc_state.get_zoom()*1.25f);
#endif
}

void
EditorMapComponent::zoom_to(Rectf rect)
{
  m_gc_state.zoom_to(rect);
}

Rectf
EditorMapComponent::get_clip_rect() const
{
  return m_gc_state.get_clip_rect();
}

void
EditorMapComponent::move_to(int x, int y)
{
  m_gc_state.set_pos(Pointf(x, y));
}

void
EditorMapComponent::move_to_x(float x)
{
  m_gc_state.set_pos(Pointf(x, m_gc_state.get_pos().y));
}

void
EditorMapComponent::move_to_y(float y)
{
  m_gc_state.set_pos(Pointf(m_gc_state.get_pos().x, y));
}

#ifdef GRUMBEL
void
EditorMapComponentImpl::on_resize(int old_w, int old_h)
{
  Rect rect = parent->get_screen_rect();

  scrollbar_v->set_position(rect.get_width() - 14 + rect.left,  2 + rect.top);
  scrollbar_v->set_size(12, rect.get_height() - 4 - 14);

  scrollbar_h->set_position(2 + rect.left, rect.get_height() - 14 + rect.top);
  scrollbar_h->set_size(rect.get_width() - 4 - 14, 12);

  gc_state.set_size(rect.get_width(), rect.get_height());
}
#endif

#ifdef GRUMBEL
boost::signals2::signal<void (int, int)>&
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
#endif

GraphicContextState&
EditorMapComponent::get_gc_state()
{
  return m_gc_state;
}

/* EOF */
