//  $Id: keyboard_controller.cxx,v 1.6 2003/09/20 21:53:38 grumbel Exp $
//
//  Windstille - A Jump'n Shoot Game
//  Copyright (C) 2000 Ingo Ruhnke <grumbel@gmx.de>
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

#include <ClanLib/display.h>
#include "keyboard_controller.hxx"

KeyboardController::KeyboardController()
{
  slots.connect(CL_Keyboard::sig_key_down(), this, &KeyboardController::on_key_down);
  slots.connect(CL_Keyboard::sig_key_up(),   this, &KeyboardController::on_key_up);
}

KeyboardController::~KeyboardController()
{
}

void
KeyboardController::on_key_down(const CL_InputEvent& event)
{
  switch (event.id)
    {
    case CL_KEY_UP:
      send_event(InputEvent::JUMP, true);
      break;
    case CL_KEY_LEFT:
      send_event(InputEvent::LEFT, true);
      break;
    case CL_KEY_RIGHT:
      send_event(InputEvent::RIGHT, true);
      break;
    case CL_KEY_DOWN:
      send_event(InputEvent::DOWN, true);
      break;
    case CL_KEY_LCONTROL:
      send_event(InputEvent::FIRE, true);
      break;
    }
}

void
KeyboardController::on_key_up(const CL_InputEvent& event)
{
   switch (event.id)
    {
    case CL_KEY_UP:
      send_event(InputEvent::JUMP, false);
      break;
    case CL_KEY_LEFT:
      send_event(InputEvent::LEFT, false);
      break;
    case CL_KEY_RIGHT:
      send_event(InputEvent::RIGHT, false);
      break;
    case CL_KEY_DOWN:
      send_event(InputEvent::DOWN, false);
      break;
    case CL_KEY_LCONTROL:
      send_event(InputEvent::FIRE, false);
      break;
    }
}

void
KeyboardController::update(float delta)
{
  
}

/* EOF */
