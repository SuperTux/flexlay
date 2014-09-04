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

#include <iostream>

#include "box.hpp"
#include "color.hpp"
#include "display.hpp"
#include "globals.hpp"
#include "helper.hpp"
#include "icon.hpp"
#include "math/point.hpp"
#include "math/rect.hpp"
#include "math/size.hpp"
#include "titlebar.hpp"
#include "window.hpp"

class WindowImpl
{
public:
  CL_Component* client_area;
  CL_Component* parent;

  Rect old_position;
  bool is_maximized;

  Titlebar* titlebar;
  Icon* close;
  Icon* minimize;
  Icon* maximize;

  std::vector<CL_Slot> slots;

  void draw();
  void do_maximize();
  void do_close();
  void on_resize(int, int);
};

Window::Window(const Rect& rect, const std::string& title, CL_Component* parent)
  : CL_Component(rect.to_cl(), parent), impl(new WindowImpl())
{
  impl->titlebar = new Titlebar(Rect(Point(3+16,3),
                                     Size(get_width()-6-18-18-18, 12+3)), title,
                                this);
  //Fonts::verdana11.draw(8+15, 3, title);

  impl->close = new Icon(Rect(Point(3, 3), Size(18,18)),
                         make_sprite(datadir + "/images/window/close.png"),
                         "", this);
  impl->minimize = new Icon(Rect(Point(get_width()-3-18-18, 3), Size(18,18)),
                            make_sprite(datadir + "/images/window/minimize.png"),
                            "", this);
  impl->maximize = new Icon(Rect(Point(get_width()-3-18, 3), Size(18,18)),
                            make_sprite(datadir + "/images/window/maximize.png"),
                            "", this);

  impl->client_area = new CL_Component(Rect(Point(4, 3+12+7),
                                            Size(rect.get_width()-10,
                                                 rect.get_height()-28)).to_cl(), this);
  impl->parent  = this;
  impl->is_maximized = false;

  impl->slots.push_back(sig_resize().connect(impl.get(), &WindowImpl::on_resize));

  impl->slots.push_back(sig_paint().connect(impl.get(), &WindowImpl::draw));
  impl->maximize->sig_clicked().connect(std::bind(&WindowImpl::do_maximize, impl.get()));
  impl->close->sig_clicked().connect(std::bind(&WindowImpl::do_close, impl.get()));
}

Window::~Window()
{
  std::cout << "deleting: Window" << std::endl;
}

void
WindowImpl::on_resize(int, int)
{
  titlebar->set_position(Rect(Point(3+16,3), Size(parent->get_width()-6-18-18-18, 12+3)).to_cl());
  close->set_position(3, 3);
  minimize->set_position(parent->get_width()-3-18-18, 3);
  maximize->set_position(parent->get_width()-3-18, 3);
  Rect rect = parent->get_position();
  client_area->set_position(Rect(Point(4, 3+12+7),
                                 Size(rect.get_width()-10,
                                      rect.get_height()-28)).to_cl());
}

void
WindowImpl::draw()
{
  Display::push_modelview();
  Display::add_translate(parent->get_screen_x(), parent->get_screen_y());

  Color highlight(255, 255, 255);
  Color midtone(150, 150, 150);

  Rect rect = parent->get_position() ;

  Box::draw_window(Rect(Point(0, 0), Size(rect.get_width()-1, rect.get_height()-1)));
  Box::draw_panel_down(Rect(client_area->get_position()));

  /*
    Display::fill_rect(Rect(Point(0, 0), rect.get_size()).to_cl(), Color(220, 220, 220));
    Display::draw_rect(Rect(Point(0, 0), rect.get_size()).to_cl(), Color(0, 0, 0));

    Display::draw_line(1, rect.get_height()-2,
    rect.get_width()-2, rect.get_height()-2, midtone);
    Display::draw_line(rect.get_width()-2, 1,
    rect.get_width()-2, rect.get_height()-2, midtone);

    Display::draw_line(1, 1,
    rect.get_width()-2, 1, highlight);
    Display::draw_line(1, 1,
    1, rect.get_height()-2, highlight);
  */

  Display::pop_modelview();
}

void
WindowImpl::do_close()
{
  parent->show(false);
}

void
WindowImpl::do_maximize()
{
  // FIXME: Move this to scripting language
  if (!is_maximized)
  {
    is_maximized = true;
    old_position = parent->get_position();
    parent->set_position(parent->get_parent()->get_position());
  }
  else
  {
    is_maximized = false;
    parent->set_position(old_position.to_cl());
  }
}

CL_Component*
Window::get_client_area()
{
  return impl->client_area;
}

void
Window::hide()
{
  CL_Component::show(false);
}

void
Window::show()
{
  CL_Component::show(true);
}

/* EOF */
