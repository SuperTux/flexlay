//  $Id: view_component.cxx,v 1.2 2003/10/12 11:58:09 grumbel Exp $
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

#include <guile/gh.h>
#include <ClanLib/Display/display.h>
#include <ClanLib/Display/keyboard.h>
#include <ClanLib/Display/display_window.h>
#include <ClanLib/Display/input_context.h>
#include "view.hxx"
#include "fonts.hxx"
#include "view_component.hxx"

ViewComponent::ViewComponent(CL_Component* parent, View* v)
  : CL_Component(CL_Rect(CL_Point(0,0), CL_Size(800, 600)), parent),
    view(v)
{
  slots.push_back(sig_key_down().connect(this, &ViewComponent::on_input_down));
  slots.push_back(sig_key_up().connect(this, &ViewComponent::on_input_up));

  slots.push_back(sig_mouse_down().connect(this, &ViewComponent::on_input_down));
  slots.push_back(sig_mouse_up().connect(this, &ViewComponent::on_input_up));

  slots.push_back(sig_paint().connect(this, &ViewComponent::draw));
}

void
ViewComponent::on_input_down(const CL_InputEvent& event)
{
  if (event.device.get_type() == CL_InputDevice::mouse)
    {
      CL_Pointf pos = view->screen2world(CL_Pointf(event.mouse_pos.x,
                                                   event.mouse_pos.y));
      gh_call2(gh_lookup("*mouse-down-handler*"), 
               gh_double2scm(pos.x), gh_double2scm(pos.y));
    }
  else if (event.device.get_type() == CL_InputDevice::keyboard)
    {
      gh_call1(gh_lookup("*key-down-handler*"), 
               gh_str02scm(CL_Keyboard::get_device().keyid_to_string(event.id).c_str()));
    }
}

void
ViewComponent::on_input_up(const CL_InputEvent& event)
{
  if (event.device.get_type() == CL_InputDevice::mouse)
    {
      CL_Pointf pos = view->screen2world(CL_Pointf(event.mouse_pos.x,
                                                   event.mouse_pos.y));
      gh_call2(gh_lookup("*mouse-up-handler*"), 
               gh_double2scm(pos.x), gh_double2scm(pos.y));
    }
}

void
ViewComponent::draw()
{
  Fonts::dialog.set_alignment(origin_bottom_left);
  Fonts::dialog.draw(10, get_height()-10, "..:: Debug ::..");
}

/* EOF */
