// Flexlay - A Generic 2D Game Editor
// Copyright (C) 2014 Ingo Ruhnke <grumbel@gmail.com>
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <http://www.gnu.org/licenses/>.

#ifndef HEADER_INPUT_EVENT_HPP
#define HEADER_INPUT_EVENT_HPP

#include "math/point.hpp"

class CL_InputEvent;

class InputEvent
{
private:
public:
  enum Type {
    MOUSE_LEFT,
    MOUSE_MIDDLE,
    MOUSE_RIGHT,
    MOUSE_WHEEL_UP,
    MOUSE_WHEEL_DOWN
  };

  enum Modifier {
    MOD_SHIFT = (1<<0)
  };

  InputEvent();

  Type id;
  Point mouse_pos;
  unsigned int mod;
};

#endif

/* EOF */
