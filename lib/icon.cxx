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
#include <ClanLib/Display/display_iostream.h>
#include "box.hxx"
#include "fonts.hxx"
#include "icon.hxx"

class IconImpl
{
public:
  IconImpl(Icon* p) : parent(p) {}

  Icon* parent;

  std::vector<CL_Slot> slots;
  CL_Sprite sprite;
  std::string tooltip;
  bool draw_tooltip;
  bool down;

  /** Parameter to keep the button down all the time, aka togglebutton
      like */
  bool is_down;

  bool is_enabled;
  CL_Signal_v0 sig_on_click;

  void draw();

  void mouse_up  (const CL_InputEvent& event);
  void mouse_down(const CL_InputEvent& event);
  void mouse_move(const CL_InputEvent& event);
};

Icon::Icon(const CL_Rect& rect, const CL_Sprite& sprite, const std::string& tooltip, 
           CL_Component* parent)
  : CL_Component(rect, parent),
    impl(new IconImpl(this))
{
  impl->sprite       = sprite;
  impl->tooltip      = tooltip;
  impl->draw_tooltip = true;
  impl->down         = false;
  impl->is_down      = false;
  impl->is_enabled   = true;

  impl->slots.push_back(sig_paint().connect(impl.get(), &IconImpl::draw));
  impl->slots.push_back(sig_mouse_down().connect(impl.get(), &IconImpl::mouse_down));
  impl->slots.push_back(sig_mouse_up().connect(impl.get(),   &IconImpl::mouse_up));
}

CL_Signal_v0&
Icon::sig_clicked()
{
  return impl->sig_on_click;
}
  
void
IconImpl::draw()
{
  CL_Display::push_translate(parent->get_screen_x(), parent->get_screen_y());
  CL_Rect rect(CL_Point(0, 0), CL_Size(parent->get_width()-4, parent->get_height()-4));

  sprite.set_alignment(origin_center);

  if (is_enabled)
    {
      if (is_down)
        {
          Box::draw_button_down(rect);
        }
      else
        {
          if (parent->has_mouse_over())
            {
              if (down)
                Box::draw_button_down(rect);
              else
                Box::draw_button_up(rect);
            }
          else
            Box::draw_button_neutral(rect);
        }
      sprite.set_alpha(1.0f);
    }
  else
    {
      Box::draw_button_neutral(rect);
      sprite.set_alpha(0.3f);
    }
  sprite.draw((rect.get_width()+1)/2, (rect.get_height()+1)/2);
  CL_Display::pop_modelview();
}

void
IconImpl::mouse_up  (const CL_InputEvent& event)
{
  if (is_enabled)
    {
      down         = false;
      parent->release_mouse();  

      if (parent->has_mouse_over())
        {
          sig_on_click();
        }
    }
}

void
IconImpl::mouse_down(const CL_InputEvent& event)
{
  if (is_enabled)
    {
      down         = true;
      parent->capture_mouse();
    }
}

void 
IconImpl::mouse_move(const CL_InputEvent& event)
{
  //std::cout << "icon: mouse_move: " << event << std::endl;
}

void
Icon::disable()
{
  impl->is_enabled = false;
}

void
Icon::enable()
{
  impl->is_enabled = true;
}

void
Icon::set_up()
{
  impl->is_down = false;
}

void
Icon::set_down()
{
  impl->is_down = true;
}

/* EOF */
