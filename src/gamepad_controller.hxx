//  $Id: gamepad_controller.hxx,v 1.1 2003/08/10 19:56:40 grumbel Exp $
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

#ifndef GAMEPADCONTROLLER_HXX
#define GAMEPADCONTROLLER_HXX

#include <ClanLib/display.h>
#include "controller.hxx"

class GamepadController : public Controller
{
private:
  CL_InputAxis* x_axis;
  CL_InputAxis* y_axis;

  CL_InputButton* fire_button;
  CL_InputButton* jump_button;
  CL_InputButton* surround_button;
  CL_InputButton* smartbomb_button;
public:
  GamepadController (unsigned int joy_num);

  // Directional Pad
  bool is_right ();
  bool is_left ();
  bool is_up ();
  bool is_down ();

  // Buttons
  bool fire_pressed ();
  bool jump_pressed ();
  bool surround_pressed ();
  bool smartbomb_pressed ();
};

#endif

/* EOF */
