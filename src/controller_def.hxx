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

#ifndef HEADER_CONTROLLER_DEF_HXX
#define HEADER_CONTROLLER_DEF_HXX

#include <string>

//enum AxisName       { ORIENTATION_AXIS, ACCELERATE_AXIS, STRAFE_AXIS };
enum ButtonName     { UP_BUTTON, DOWN_BUTTON, LEFT_BUTTON, RIGHT_BUTTON, FIRE_BUTTON, JUMP_BUTTON };

/** */
class ControllerDef
{
private:
public:
  static int         get_button_count();
  static int         get_axis_count();

  static std::string button_id2name(int id);
  static int         button_name2id(const std::string& name);

  static std::string axis_id2name(int id);
  static int         axis_name2id(const std::string& name);
};

#endif

/* EOF */
