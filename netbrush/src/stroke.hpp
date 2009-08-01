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

#ifndef HEADER_STROKE_HPP
#define HEADER_STROKE_HPP

#include "SDL.h"
#include <vector>
#include "math/vector.hpp"
#include "math/rect.hpp"

/** A dab is basically an event send from the mouse to the drawing
    canvas, it consists of time, position, tilt, pressure and possible
    additional information that is needed */
class Dab
{
public:
  /** Time at which the dot was placed */
  unsigned int time;

  /** Position at which the dot is placed */
  Vector pos;

  /** The pressure with which the dot was drawn (can be interpreted as
      size, opacity or similar things by the StrokeDrawer */
  float pressure;

  /** Tilting of the pen while painting the dot */
  Vector tilt;

  Dab()
    : time(SDL_GetTicks()), pos(0, 0), pressure(1.0f), tilt(0, 0)
  {}

  Dab(float x, float y) 
    : time(SDL_GetTicks()), pos(x, y), pressure(1.0f), tilt(0.0f, 0.0f)
  {}

  Dab(float x_, float y_, float pressure_)
    : time(SDL_GetTicks()), pos(x_, y_), pressure(pressure_), tilt(0.0f, 0.0f)
  {}
};

/** */
class Stroke
{
public:
  typedef std::vector<Dab> Dabs;

private:
  Stroke::Dabs dabs;
  Rect bounding_rect;

public:
  Stroke();
  ~Stroke();

  /** Return true if the Stroke doesn't contain any dabs */
  bool empty() const { return dabs.empty(); }

  void  add_dab(const Dab& dab);
  
  /** Returns the real dabs as recieved by the InputDevice */
  const Dabs&  get_dabs()  const;

  /** Returns interpolated dabs, meaning the holes in get_dabs() are
      closed with interpolated dabs so that all dabs are equally
      spread (ie. every dab is 'spacing' away from the next) */
  Dabs  get_interpolated_dabs(float x_spacing, float y_spacing) const;

  int get_dab_count() const;

  const Rect& get_bounding_rect() const;

private:
  Stroke (const Stroke&);
  Stroke& operator= (const Stroke&);
};

class DabInterpolater
{
private:
  float x_spacing;
  float y_spacing;
  float overspace;

  Stroke::Dabs interpolated_dabs;
  Stroke::Dabs dabs;

public: 
  DabInterpolater(float x_spacing_, float y_spacing_);

  void add_dab(const Dab& dab);
  const Stroke::Dabs& get_interpolated_dabs() const;
};

#endif

/* EOF */
