//  $Id: gamepad_controller.hxx,v 1.3 2003/10/29 15:34:43 grumbel Exp $
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

#ifndef GAMEPADCONTROLLER_HXX
#define GAMEPADCONTROLLER_HXX

#include <ClanLib/display.h>
#include "controller.hxx"

class GamepadController : public Controller
{
private:
  CL_SlotContainer slots;

  void on_key_down(const CL_InputEvent&);
  void on_key_up(const CL_InputEvent&);
  void on_axis_move(const CL_InputEvent&);
public:
  GamepadController(int num);
  virtual ~GamepadController();

  void update(float delta);
};

#endif

/* EOF */
