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

#include <ClanLib/Display/display.h>
#include <ClanLib/Display/keys.h>
#include "math.hxx"
#include "slider.hxx"

Slider::Slider(const CL_Rect& rect, CL_Component* parent)
  : CL_Component(rect, parent)
{
  start = 0.0f;
  end   = 100.0f;
  value = 50.0f;
  pressed = false;

  slots.push_back(sig_mouse_down().connect(this, &Slider::on_mouse_down));
  slots.push_back(sig_mouse_up().connect(this, &Slider::on_mouse_up));
  slots.push_back(sig_mouse_move().connect(this, &Slider::on_mouse_move));
  slots.push_back(sig_paint().connect(this, &Slider::draw));
}

Slider::~Slider()
{
}

void
Slider::draw()
{
  CL_Display::push_modelview();
  CL_Display::add_translate(get_screen_x(), get_screen_y());
    
  CL_Display::fill_rect(CL_Rect(CL_Point(0, get_height()/2 - 2),
                                CL_Size(get_width(), 5)),
                        CL_Color(255, 255, 255, 255));

  CL_Display::fill_rect(CL_Rect(CL_Point(int(-2 + (value/(end-start)) * get_width()), 0),
                                CL_Size(5, get_height())),
                        CL_Color(0, 0, 0, 255));

  CL_Display::pop_modelview();    
}

void
Slider::set_range(float start_, float end_)
{
  start = start_;
  end   = end_;
}

CL_Signal_v1<float>&
Slider::sig_on_change()
{
  return on_change;
}

void
Slider::set_value(float value_)
{
  value = value_;
  on_change(value);
}

void
Slider::update_mouse(const CL_InputEvent& event)
{
  set_value(Math::mid(start, (float(event.mouse_pos.x) / get_width()) * (end - start), end));
}

void
Slider::on_mouse_down(const CL_InputEvent& event)
{
  if (event.id == CL_MOUSE_LEFT)
    {
      pressed = true;
      capture_mouse();
      update_mouse(event);
    }
}

void
Slider::on_mouse_up  (const CL_InputEvent& event)
{
  if (event.id == CL_MOUSE_LEFT)
    {
      pressed = false;
      release_mouse();
      update_mouse(event);
    }
}

void
Slider::on_mouse_move(const CL_InputEvent& event)
{
  if (pressed)
    {
      update_mouse(event);
    }
}

/* EOF */
