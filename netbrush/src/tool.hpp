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

#ifndef HEADER_TOOL_HPP
#define HEADER_TOOL_HPP

#include "SDL.h"
#include "math/point.hpp"
#include "math/rect.hpp"
#include "widget/events.hpp"

enum ToolName { PAINTBRUSH_TOOL, RECT_TOOL, REGION_TOOL, COLOR_PICKER_TOOL, CIRCLE_TOOL, LINE_TOOL };

struct ToolMotionEvent
{
  int x;
  int y;

  Point screen;
};


struct ToolButtonEvent
{
  int x;
  int y;

  Point screen;
};

/** */
class Tool
{
private:
public:
  Tool() {}
  virtual ~Tool() {}

  virtual void on_motion(const ToolMotionEvent& ev) =0;
  virtual void on_button_press(const ToolButtonEvent& ev) =0;
  virtual void on_button_release(const ToolButtonEvent& ev) =0;
  virtual void on_pen_motion(const PenEvent& pen) {}

  /** @param target SDL_Surface to which should be drawn 
      @param rect   rectangle in screenspace which should be redrawn
      @param x_of   scroll factor used to translate from screenspace to worldspace
      @param y_of   scroll factor used to translate from screenspace to worldspace
   */
  virtual void draw(SDL_Surface* target, const Rect& rect, int x_of, int y_of) =0;

private:
  Tool (const Tool&);
  Tool& operator= (const Tool&);
};

#endif

/* EOF */
