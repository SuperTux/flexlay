// Flexlay - A Generic 2D Game Editor
// Copyright (C) 2002 Ingo Ruhnke <grumbel@gmail.com>
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

#ifndef HEADER_FLEXLAY_STROKE_HPP
#define HEADER_FLEXLAY_STROKE_HPP

#include <vector>
#include <memory>

#include "math/point.hpp"
#include "math/rect.hpp"

class GraphicContext;
class StrokeImpl;
class StrokeDrawer;
class CL_GraphicContext;

/** A dab is basically an event send from the mouse to the drawing
    canvas, it consists of time, position, tilt, pressure and possible
    additional information that is needed */
class Dab
{
public:
  /** Time at which the dot was placed */
  unsigned int time;

  /** Position at which the dot is placed */
  Pointf pos;

  /** The pressure with which the dot was drawn (can be interpreted as
      size, opacity or similar things by the StrokeDrawer */
  float pressure;

  /** Tilting of the pen while painting the dot */
  Pointf tilt;

  Dab()
#ifdef GRUMBEL
    : time(CL_System::get_time()), pos(0, 0), pressure(1.0f), tilt(0, 0)
#endif
  {}

  Dab(float x, float y)
#ifdef GRUMBEL
    : time(CL_System::get_time()), pos(x, y), pressure(1.0f), tilt(0.0f, 0.0f)
#endif
  {}

  Dab(float x_, float y_, float pressure_)
#ifdef GRUMBEL
    : time(CL_System::get_time()), pos(x_, y_), pressure(pressure_), tilt(0.0f, 0.0f)
#endif
  {}
};

/** A Stroke is a series of Dabs */
class Stroke
{
public:
  typedef std::vector<Dab> Dabs;

  Stroke();

  /** Return true if the Stroke doesn't contain any dabs */
  bool empty() const;

  void draw(GraphicContext& gc) const;

  void  set_drawer(const StrokeDrawer& drawer_);
  StrokeDrawer get_drawer();
  void  add_dab(const Dab& dab);

  /** Returns the real dabs as recieved by the InputDevice */
  Dabs  get_dabs()  const;

  /** Returns interpolated dabs, meaning the holes in get_dabs() are
      closed with interpolated dabs so that all dabs are equally
      spread (ie. every dab is 'spacing' away from the next) */
  Dabs  get_interpolated_dabs(float x_spacing, float y_spacing) const;

  int get_dab_count() const;

  Rectf get_bounding_rect() const;
private:
  std::shared_ptr<StrokeImpl> impl;
};

#endif

/* EOF */
