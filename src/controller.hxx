//  $Id: controller.hxx,v 1.4 2003/09/21 17:34:00 grumbel Exp $
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

#ifndef CONTROLLER_HXX
#define CONTROLLER_HXX

#include <vector>
#include "input_event.hxx"

class Controller
{
public:
  typedef std::vector<InputEvent> Events;
private:
  std::vector<bool> states;
  Events events;
protected:
  void send_event(int type, bool state);

  static Controller* current_;
public:
  static Controller* current() { return current_; }

  Controller();
  virtual ~Controller();

  Events& get_events() { return events; }

  bool get_state(int type);
  void clear();

  virtual void update(float delta) =0;
};

#endif

/* EOF */
