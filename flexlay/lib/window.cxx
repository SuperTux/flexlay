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
#include <ClanLib/Display/sprite_description.h>
#include <ClanLib/Display/Providers/provider_factory.h>
#include <ClanLib/GUI/gui_manager.h>
#include "box.hxx"
#include "fonts.hxx"
#include "icon.hxx"
#include "helper.hxx"
#include "titlebar.hxx"
#include "window.hxx"

class WindowImpl
{
public:
  CL_Component* client_area;
  CL_Component* parent;

  CL_Rect old_position;
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

Window::Window(const CL_Rect& rect, const std::string& title, CL_Component* parent)
  : CL_Component(rect, parent), impl(new WindowImpl())
{
  impl->titlebar = new Titlebar(CL_Rect(CL_Point(3+16,3), 
                                        CL_Size(get_width()-6-18-18-18, 12+3)), title,
                                this);
  //Fonts::verdana11.draw(8+15, 3, title);

  impl->close = new Icon(CL_Rect(CL_Point(3, 3), CL_Size(18,18)), 
                        make_sprite("../data/images/window/close.png"),
                        "", this);
  impl->minimize = new Icon(CL_Rect(CL_Point(get_width()-3-18-18, 3), CL_Size(18,18)), 
                            make_sprite("../data/images/window/minimize.png"),
                            "", this);
  impl->maximize = new Icon(CL_Rect(CL_Point(get_width()-3-18, 3), CL_Size(18,18)), 
                            make_sprite("../data/images/window/maximize.png"),
                            "", this);

  impl->client_area = new CL_Component(CL_Rect(CL_Point(4, 3+12+7), 
                                               CL_Size(rect.get_width()-10,
                                                       rect.get_height()-28)), this);
  impl->parent  = this;
  impl->is_maximized = false;

  impl->slots.push_back(sig_resize().connect(impl.get(),      &WindowImpl::on_resize));

  impl->slots.push_back(sig_paint().connect(impl.get(),      &WindowImpl::draw));
  impl->slots.push_back(impl->maximize->sig_clicked().connect(impl.get(), &WindowImpl::do_maximize));
  impl->slots.push_back(impl->close->sig_clicked().connect(impl.get(), &WindowImpl::do_close));
}

Window::~Window()
{
  std::cout << "deleting: Window" << std::endl;
}

void
WindowImpl::on_resize(int, int)
{
  titlebar->set_position(CL_Rect(CL_Point(3+16,3), CL_Size(parent->get_width()-6-18-18-18, 12+3)));
  close->set_position(3, 3);
  minimize->set_position(parent->get_width()-3-18-18, 3);
  maximize->set_position(parent->get_width()-3-18, 3);
  CL_Rect rect = parent->get_position();
  client_area->set_position(CL_Rect(CL_Point(4, 3+12+7), 
                                   CL_Size(rect.get_width()-10,
                                           rect.get_height()-28)));
}

void
WindowImpl::draw()
{
  CL_Display::push_translate (parent->get_screen_x(), parent->get_screen_y());

  CL_Color highlight(255, 255, 255);
  CL_Color midtone(150, 150, 150);

  CL_Rect rect = parent->get_position() ;

  Box::draw_window(CL_Rect(CL_Point(0, 0), CL_Size(rect.get_width()-1, rect.get_height()-1)));
  Box::draw_panel_down(client_area->get_position());

  /*
    CL_Display::fill_rect(CL_Rect(CL_Point(0, 0), rect.get_size()), CL_Color(220, 220, 220));
    CL_Display::draw_rect(CL_Rect(CL_Point(0, 0), rect.get_size()), CL_Color(0, 0, 0));
 
    CL_Display::draw_line(1, rect.get_height()-2,
    rect.get_width()-2, rect.get_height()-2, midtone);
    CL_Display::draw_line(rect.get_width()-2, 1,
    rect.get_width()-2, rect.get_height()-2, midtone);

    CL_Display::draw_line(1, 1,
    rect.get_width()-2, 1, highlight);
    CL_Display::draw_line(1, 1,
    1, rect.get_height()-2, highlight);
  */

  CL_Display::pop_modelview();
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
      parent->set_position(old_position);
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
