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
#include "window.hxx"

CL_Sprite
make_sprite(const std::string& filename)
{
  CL_SpriteDescription desc;
  desc.add_frame(CL_ProviderFactory::load(filename), true);
  return CL_Sprite(desc);
}

class WindowImpl
{
public:
  CL_Component* client_area;
  CL_Component* parent;

  CL_Component* close;
  CL_Component* minimize;
  CL_Component* maximize;

  std::vector<CL_Slot> slots;
  CL_Point click_pos;
  CL_Rect  old_pos;
  bool pressed;
  std::string title;

  void draw();
  void mouse_down(const CL_InputEvent& event);
  void mouse_up(const CL_InputEvent& event);
  void mouse_move(const CL_InputEvent& event);
};

Window::Window(const CL_Rect& rect, const std::string& title, CL_Component* parent)
  : CL_Component(rect, parent), impl(new WindowImpl())
{
  impl->close = new Icon(CL_Rect(CL_Point(3, 3), CL_Size(18,18)), 
                        make_sprite("../data/images/window/close.png"),
                        "", this);
  impl->minimize = new Icon(CL_Rect(CL_Point(get_width()-3-18-18, 3), CL_Size(18,18)), 
                            make_sprite("../data/images/window/minimize.png"),
                            "", this);
  impl->maximize = new Icon(CL_Rect(CL_Point(get_width()-3-18, 3), CL_Size(18,18)), 
                            make_sprite("../data/images/window/maximize.png"),
                            "", this);

  impl->client_area = new CL_Component(CL_Rect(CL_Point(3, 3+12+6), 
                                               CL_Size(rect.get_width()-8,
                                                       rect.get_height()-25)), this);
  impl->parent = this;
  impl->pressed = false;
  impl->title = title;
 
  impl->slots.push_back(sig_paint().connect(impl.get(),      &WindowImpl::draw));
  impl->slots.push_back(sig_mouse_down().connect(impl.get(), &WindowImpl::mouse_down));
  impl->slots.push_back(sig_mouse_up().connect(impl.get(),   &WindowImpl::mouse_up));
  impl->slots.push_back(sig_mouse_move().connect(impl.get(),   &WindowImpl::mouse_move));
}

Window::~Window()
{
  delete impl->client_area;
}

void
WindowImpl::draw()
{
  CL_Color highlight(255, 255, 255);
  CL_Color midtone(150, 150, 150);

  CL_Rect rect = parent->get_position() ;

  Box::draw_panel(CL_Rect(CL_Point(0, 0), CL_Size(rect.get_width()-1, rect.get_height()-1)));

  CL_Display::fill_rect(CL_Rect(CL_Point(3+16,3), CL_Size(parent->get_width()-6-18-18-18, 12+3)), CL_Color(250, 250, 250));
  Fonts::verdana11.draw(8+15, 3, title);

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
}

void
WindowImpl::mouse_down(const CL_InputEvent& event)
{
  if (event.id == CL_MOUSE_MIDDLE)
    {
      pressed = true;
      click_pos = event.mouse_pos;
      parent->capture_mouse();

      old_pos = parent->get_position();
      click_pos.x += old_pos.left;
      click_pos.y += old_pos.top;
    }
}

void
WindowImpl::mouse_up(const CL_InputEvent& event)
{
  if (event.id == CL_MOUSE_MIDDLE)
    {
      pressed = false;
      parent->release_mouse();

      parent->raise();
    }
}

void
WindowImpl::mouse_move(const CL_InputEvent& event)
{
  if(pressed)
    {
      CL_Rect rect = parent->get_position();

      CL_Point move(old_pos.left - (click_pos.x - (rect.left + event.mouse_pos.x)), 
                    old_pos.top  - (click_pos.y - (rect.top  + event.mouse_pos.y)));

      parent->set_position(move.x, move.y);
    }
}

CL_Component*
Window::get_client_area()
{
  return impl->client_area;
}

/* EOF */
