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

#include <ClanLib/Display/display.h>
#include <ClanLib/Display/keys.h>
#include <ClanLib/Display/mouse.h>

#include "color.hpp"
#include "display.hpp"
#include "fonts.hpp"
#include "math/rect.hpp"
#include "titlebar.hpp"

class TitlebarImpl
{
public:
  CL_Component* window;
  Titlebar* parent;
  Point click_pos;
  Rect old_pos;
  std::string title;
  std::vector<CL_Slot> slots;
  bool pressed;

  TitlebarImpl(Titlebar* parent_) : parent(parent_) {}

  void on_mouse_move(const CL_InputEvent& event);
  void on_mouse_down(const CL_InputEvent& event);
  void on_mouse_up(const CL_InputEvent& event);
  void draw();
};

Titlebar::Titlebar(const Rect& rect, const std::string& title, CL_Component* parent) :
  CL_Component(rect.to_cl(), parent),
  impl(new TitlebarImpl(this))
{
  impl->title = title;
  impl->pressed = false;
  impl->window = parent;

  impl->slots.push_back(sig_mouse_down().connect(impl.get(), &TitlebarImpl::on_mouse_down));
  impl->slots.push_back(sig_mouse_move().connect(impl.get(), &TitlebarImpl::on_mouse_move));
  impl->slots.push_back(sig_mouse_up().connect(impl.get(), &TitlebarImpl::on_mouse_up));
  impl->slots.push_back(sig_paint().connect(impl.get(), &TitlebarImpl::draw));
}

void
TitlebarImpl::on_mouse_up(const CL_InputEvent& event)
{
  if (event.id == CL_MOUSE_LEFT)
  {
    pressed = false;
    parent->release_mouse();
  }
}

void
TitlebarImpl::on_mouse_down(const CL_InputEvent& event)
{
  if (event.id == CL_MOUSE_LEFT)
  {
    pressed   = true;
    click_pos = event.mouse_pos;
    parent->capture_mouse();
    window->raise();

    old_pos = window->get_position();
    click_pos.x += old_pos.left;
    click_pos.y += old_pos.top;
  }
}

void
TitlebarImpl::on_mouse_move(const CL_InputEvent& event)
{
  if(pressed)
  {
    Rect rect = window->get_position();

    Point move(old_pos.left - (click_pos.x - (rect.left + event.mouse_pos.x)),
                  old_pos.top  - (click_pos.y - (rect.top  + event.mouse_pos.y)));

    window->set_position(move.x, move.y);
  }
}

void
TitlebarImpl::draw()
{
  Display::push_modelview();
  Display::add_translate(parent->get_screen_x(), parent->get_screen_y());

  // FIXME: Hack should be done via has_mouse_over(), but that doesn't include child components
  if (parent->get_parent()->get_position().is_inside(Point(CL_Mouse::get_x(),
                                                           CL_Mouse::get_y()).to_cl()))
    //parent->get_parent()->has_mouse_over())
  {
    Display::fill_rect(Rect(Point(0, 0),
                               Size(parent->get_width()-1, parent->get_height())),
                          Color(250, 250, 250));
  }
  else
  {
    Display::fill_rect(Rect(Point(0, 0),
                               Size(parent->get_width()-1, parent->get_height())),
                          Color(240, 240, 240));
  }

  Fonts::verdana11.draw(4, 0, title);

  Display::pop_modelview();
}

/* EOF */
