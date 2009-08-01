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

#ifndef HEADER_SCREEN_BUFFER_HPP
#define HEADER_SCREEN_BUFFER_HPP

#include "SDL.h"
#include "tool.hpp"
#include "widget/widget.hpp"

class Tool;

class ColorpickerTool;
class ScrollTool;
class RegionTool;
class RectTool;
class CircleTool;
class LineTool;
class AirbrushTool;

/** */
class ScreenBuffer : public Widget
{
private:
  //SDL_Surface* buffer;
  Rect dirty_region;

  bool complete_refresh;

  int scroll_offset_x;
  int scroll_offset_y;

  ColorpickerTool* colorpicker_tool;
  RegionTool*      region_tool;
  RectTool*        rect_tool;
  CircleTool*      circle_tool;
  ScrollTool*      scroll_tool;
  AirbrushTool*    airbrush_tool;
  LineTool*    line_tool;

  typedef std::vector<Tool*> Tools;
  Tools tools;
public:
  ScreenBuffer(const Rect& rect);
  ~ScreenBuffer();

  // Mark an region as dirty in canvas space, not screen space
  void mark_dirty(int x, int y, int w, int h);
  void mark_dirty(const Rect& region);
  void force_full_refresh();
  
  void draw(GraphicContext& gc);

  void on_mouse_motion(const MouseMotionEvent& motion);
  void on_mouse_button(const MouseButtonEvent& button);
  void on_pen_motion(const PenEvent& pen);
  
  void on_enter() {}
  void on_leave() {}

  bool do_update() { return false; }

  void  move_to(const Point& p);
  Point get_pos();
  void  set_tool(ToolName tool);
private:
  ScreenBuffer (const ScreenBuffer&);
  ScreenBuffer& operator= (const ScreenBuffer&);
};

#endif

/* EOF */
