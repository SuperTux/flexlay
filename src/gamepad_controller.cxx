//  $Id: gamepad_controller.cxx,v 1.4 2003/10/29 15:34:43 grumbel Exp $
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

#include "gamepad_controller.hxx"

GamepadController::GamepadController(int num)
{
  CL_InputDevice dev = CL_Joystick::get_device(num);

  slots.connect(dev.sig_key_down(),  this, &GamepadController::on_key_down);
  slots.connect(dev.sig_key_up(),    this, &GamepadController::on_key_up);
  slots.connect(dev.sig_axis_move(), this, &GamepadController::on_axis_move);
}


GamepadController::~GamepadController()
{
}

void
GamepadController::on_key_down(const CL_InputEvent& event)
{
  if (event.id == 0)
    {
      send_event(InputEvent::JUMP, true);
    }
  else if (event.id == 1)
    {
      send_event(InputEvent::FIRE, true);
    }
}

void
GamepadController::on_key_up(const CL_InputEvent& event)
{
  if (event.id == 0)
    {
      send_event(InputEvent::JUMP, false);
    }
  else if (event.id == 1)
    {
      send_event(InputEvent::FIRE, false);
    } 
}

void
GamepadController::on_axis_move(const CL_InputEvent& event)
{
  if (event.id == 1)
    {
      if (event.axis_pos > 0.5f)
        send_event(InputEvent::DOWN, true);
      else
        send_event(InputEvent::DOWN, false);

      if (event.axis_pos < -0.5f)
        send_event(InputEvent::UP, true);
      else
        send_event(InputEvent::UP, false);
    }
  else if (event.id == 0)
    {
      if (event.axis_pos < -0.5f)
        send_event(InputEvent::LEFT, true);
      else
        send_event(InputEvent::LEFT, false);

      if (event.axis_pos > 0.5f)
        send_event(InputEvent::RIGHT, true);
      else
        send_event(InputEvent::RIGHT, false);
    }
}

void
GamepadController::update(float delta)
{
  
}

/* EOF */
