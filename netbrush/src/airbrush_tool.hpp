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

#ifndef HEADER_AIRBRUSH_TOOL_HPP
#define HEADER_AIRBRUSH_TOOL_HPP

#include "tool.hpp"

/** */
class AirbrushTool : public Tool
{
private:
  Stroke* current_stroke;
  bool pen_active;
public:
  AirbrushTool();
  ~AirbrushTool();

  void on_motion(const ToolMotionEvent& ev);
  void on_button_press(const ToolButtonEvent& ev);
  void on_button_release(const ToolButtonEvent& ev);
  void on_pen_motion(const PenEvent& pen);
  void draw(SDL_Surface* target, const Rect& rect, int x_of, int y_of) {}
private:
  AirbrushTool (const AirbrushTool&);
  AirbrushTool& operator= (const AirbrushTool&);
};

#endif

/* EOF */
