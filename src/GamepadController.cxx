//  $Id: GamepadController.cxx,v 1.1 2002/03/19 17:56:57 grumbel Exp $
//
//  Pingus - A free Lemmings clone
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

#include "GamepadController.hxx"

GamepadController::GamepadController (unsigned int joy_num)
{
  if (CL_Input::joysticks.size () > joy_num)
    {
      std::cout << "Joystick found" << std::endl;
      y_axis = CL_Input::joysticks[joy_num]->get_axis (1);
      x_axis = CL_Input::joysticks[joy_num]->get_axis (0);
      
      surround_button  = CL_Input::joysticks[joy_num]->get_button (6);
      smartbomb_button = CL_Input::joysticks[joy_num]->get_button (3);
      fire_button      = CL_Input::joysticks[joy_num]->get_button (4);
      jump_button      = CL_Input::joysticks[joy_num]->get_button (1);
    }
  else
    {
      std::cout << "No Joystick found" << std::endl;
      exit (1);
    }
}

// Directional Pad
bool 
GamepadController::is_right ()
{
  return (x_axis->get_pos () > 0.5);
}

bool
GamepadController::is_left ()
{
  return (x_axis->get_pos () < -0.5);
}

bool
GamepadController::is_up ()
{
  return (y_axis->get_pos () < -0.5);
}

bool 
GamepadController::is_down ()
{
  return (y_axis->get_pos () > 0.5);
}

// Buttons
bool 
GamepadController::fire_pressed ()
{
  return fire_button->is_pressed ();
}

bool 
GamepadController::jump_pressed ()
{
  return jump_button->is_pressed ();
}

bool 
GamepadController::surround_pressed ()
{
  return surround_button->is_pressed ();
}

bool 
GamepadController::smartbomb_pressed ()
{
  return smartbomb_button->is_pressed ();
}

/* EOF */
