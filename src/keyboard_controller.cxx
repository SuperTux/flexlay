//  $Id: keyboard_controller.cxx,v 1.1 2003/08/10 19:56:40 grumbel Exp $
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

#include <ClanLib/display.h>
#include "keyboard_controller.hxx"

// Directional Pad
bool 
KeyboardController::is_right ()
{
  return CL_Keyboard::get_keycode (CL_KEY_RIGHT);
}

bool
KeyboardController::is_left ()
{
  return CL_Keyboard::get_keycode (CL_KEY_LEFT);
}

bool 
KeyboardController::is_up ()
{
  return CL_Keyboard::get_keycode (CL_KEY_UP);
}

bool 
KeyboardController::is_down ()
{
  return CL_Keyboard::get_keycode (CL_KEY_DOWN);
}
  
// Buttons
bool
KeyboardController::fire_pressed ()
{
  return CL_Keyboard::get_keycode (CL_KEY_Q);
}

bool
KeyboardController::jump_pressed ()
{
  return CL_Keyboard::get_keycode (CL_KEY_UP);
}

bool
KeyboardController::surround_pressed ()
{
  return CL_Keyboard::get_keycode (CL_KEY_LSHIFT);
}

bool
KeyboardController::smartbomb_pressed ()
{
  return CL_Keyboard::get_keycode (CL_KEY_SPACE);
}

/* EOF */
