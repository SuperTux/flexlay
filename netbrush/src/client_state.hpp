/*            _   ___              _   
**   _ _  ___| |_| _ )_ _ _  _ _ _| |_ 
**  | ' \/ -_)  _| _ \ '_| || (_-<|   |
**  |_||_\___|\__|___/_|  \_,_/__/|_|_|
**  netBrush - Copyright (C) 2006 Ingo Ruhnke <grumbel@gmx.de>
**
**  This program is free software; you can redistribute it and/or
**  modify it under the terms of the GNU General Public License
**  as published by the Free Software Foundation; either version 2
**  of the License, or (at your option) any later version.
**
**  This program is distributed in the hope that it will be useful,
**  but WITHOUT ANY WARRANTY; without even the implied warranty of
**  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
**  GNU General Public License for more details.
** 
**  You should have received a copy of the GNU General Public License
**  along with this program; if not, write to the Free Software
**  Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
**  02111-1307, USA.
*/

#ifndef HEADER_CLIENT_STATE_HPP
#define HEADER_CLIENT_STATE_HPP

#include <string>
#include "brushmask.hpp"
#include "drawing_parameter.hpp"

class Rect;
class Point;
class Color;
class Stroke;
class DrawingParameter;

class ClientState
{
private:
  int id;
  Stroke* current_stroke;
  DrawingParameter* draw_param;

public:
  ClientState(int id_);
  ~ClientState();

  void set_tool(DrawingParameter::Tool tool);
  void set_opacity(Uint8 o);
  void set_color(const Color& color);
  void set_generic_brush(BrushShape shape,
                         float  radius,
                         int    spikes,        /* 2 - 20     */
                         float  hardness,      /* 0.0 - 1.0  */
                         float  aspect_ratio,  /* y/x (1.0f - 20.0f)       */
                         float  angle);
  void set_brush(const std::string& filename);
  void stroke_begin();
  void stroke_end();

  void copy_region(const Rect& rect, const Point& target);
  void fill_rect(const Rect& rect);
  void fill_circle(const Point& pos, int radius);
  void draw_line(const Point& p1, const Point& p2);

  void dab(unsigned int time, int x, int y, float pressure);
};

#endif

/* EOF */
