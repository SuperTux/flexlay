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
#include <ClanLib/Display/display.h>
#include <ClanLib/Display/keys.h>
#include <ClanLib/Display/mouse.h>
#include "fonts.hxx"
#include "titlebar.hxx"

class TitlebarImpl
{
public:
  CL_Component* window;
  Titlebar* parent;
  CL_Point click_pos;
  CL_Rect old_pos;
  std::string title;
  std::vector<CL_Slot> slots;
  bool pressed;

  TitlebarImpl(Titlebar* parent_) : parent(parent_) {}

  void on_mouse_move(const CL_InputEvent& event);
  void on_mouse_down(const CL_InputEvent& event);
  void on_mouse_up(const CL_InputEvent& event);
  void draw();
};

Titlebar::Titlebar(const CL_Rect& rect, const std::string& title, CL_Component* parent)
  : CL_Component(rect, parent),
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
      CL_Rect rect = window->get_position();

      CL_Point move(old_pos.left - (click_pos.x - (rect.left + event.mouse_pos.x)), 
                    old_pos.top  - (click_pos.y - (rect.top  + event.mouse_pos.y)));

      window->set_position(move.x, move.y);
    }
}

void
TitlebarImpl::draw()
{
  CL_Display::push_translate(parent->get_screen_x(), parent->get_screen_y());

  // FIXME: Hack should be done via has_mouse_over(), but that doesn't include child components
  if (parent->get_parent()->get_position().is_inside(CL_Point(CL_Mouse::get_x(), 
                                                              CL_Mouse::get_y())))
      //parent->get_parent()->has_mouse_over())
    {
      CL_Display::fill_rect(CL_Rect(CL_Point(0, 0),
                                    CL_Size(parent->get_width()-1, parent->get_height())), 
                            CL_Color(250, 250, 250));
    }
  else
    {
      CL_Display::fill_rect(CL_Rect(CL_Point(0, 0),
                                    CL_Size(parent->get_width()-1, parent->get_height())), 
                            CL_Color(240, 240, 240));
    }

  Fonts::verdana11.draw(4, 0, title);

  CL_Display::pop_modelview();
}
                  
/* EOF */
