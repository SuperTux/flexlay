//  $Id: controller.cxx,v 1.2 2003/09/21 17:34:00 grumbel Exp $
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

#include <iostream>
#include "controller.hxx"

Controller* Controller::current_ = 0;

Controller::Controller()
{
  current_ = this;
  states.resize(InputEvent::LAST_TYPE);
}

Controller::~Controller()
{
}

void
Controller::send_event(int type, bool state)
{
  if (get_state(type) == state)
    {
      std::cout << "Controller: Invalid event (nothing changed): " << type << " " << state << std::endl;
    }

  states[type] = state;
  events.push_back(InputEvent(type, state)); 
}

bool
Controller::get_state(int type)
{
  return states[type];
}

void
Controller::clear()
{
  events.clear();
}

/* EOF */
