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

#include "controller_def.hxx"

int
ControllerDef::get_button_count()
{
  return 6;
}

int
ControllerDef::get_axis_count()
{
  return 0;
}

std::string
ControllerDef::button_id2name(int id)
{
  switch (id)
    {
    case UP_BUTTON:
      return "up";
    case DOWN_BUTTON:
      return "down";
    case LEFT_BUTTON:
      return "left";
    case RIGHT_BUTTON:
      return "right";
    case FIRE_BUTTON:
      return "fire";
    case JUMP_BUTTON:
      return "jump";
    default:
      return "unknown";
    }
}

int
ControllerDef::button_name2id(const std::string& name)
{
  if (name == "up") 
    return UP_BUTTON;
  else if (name == "down")
    return DOWN_BUTTON;
  else if (name == "left")
    return LEFT_BUTTON;
  else if (name == "right")
    return (RIGHT_BUTTON);
  else if (name == "fire")
    return FIRE_BUTTON;
  else if (name == "jump")
    return JUMP_BUTTON;
  else
    return -1;
}

std::string
ControllerDef::axis_id2name(int id)
{
  return "unknown";
}

int 
ControllerDef::axis_name2id(const std::string& name)
{
  return -1;
}

/* EOF */
