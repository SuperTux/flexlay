/*            _   ___              _   
**   _ _  ___| |_| _ )_ _ _  _ _ _| |_ 
**  | ' \/ -_)  _| _ \ '_| || (_-<|   |
**  |_||_\___|\__|___/_|  \_,_/__/|_|_|
**  netBrush - Copyright (C) 2006 Ingo Ruhnke <grumbel@gmx.de>
**
**  This program is free software: you can redistribute it and/or modify
**  it under the terms of the GNU General Public License as published by
**  the Free Software Foundation, either version 3 of the License, or
**  (at your option) any later version.
**  
**  This program is distributed in the hope that it will be useful,
**  but WITHOUT ANY WARRANTY; without even the implied warranty of
**  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
**  GNU General Public License for more details.
**  
**  You should have received a copy of the GNU General Public License
**  along with this program.  If not, see <http://www.gnu.org/licenses/>.
*/

#ifndef HEADER_EVENTS_HPP
#define HEADER_EVENTS_HPP

#include "SDL.h"

class PenEvent
{
public:
  float x;
  float y;
  float pressure;
  float x_tilt;
  float y_tilt;

  PenEvent(float x, float y, float pressure, float x_tilt, float y_tilt)
    : x(x), y(y), pressure(pressure), x_tilt(x_tilt), y_tilt(y_tilt)
  {}
};

class MouseButtonEvent
{
public:
  int  button;
  int  state;
  int  x;
  int  y;

  MouseButtonEvent(const SDL_MouseButtonEvent& ev)
    : button(ev.button), state(ev.state), x(ev.x), y(ev.y)
  {}
};

class MouseMotionEvent
{
public:
  int x, y;
  int xrel, yrel;

  MouseMotionEvent(const SDL_MouseMotionEvent& ev)
    : x(ev.x), y(ev.y), xrel(ev.xrel), yrel(ev.yrel)
  {}
};

#endif

/* EOF */
