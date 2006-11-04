/*  $Id$
**   __      __ __             ___        __   __ __   __
**  /  \    /  \__| ____    __| _/_______/  |_|__|  | |  |   ____
**  \   \/\/   /  |/    \  / __ |/  ___/\   __\  |  | |  | _/ __ \
**   \        /|  |   |  \/ /_/ |\___ \  |  | |  |  |_|  |_\  ___/
**    \__/\  / |__|___|  /\____ /____  > |__| |__|____/____/\___  >
**         \/          \/      \/    \/                         \/
**  Copyright (C) 2005 Ingo Ruhnke <grumbel@gmx.de>
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

  bool pen_active;

  typedef std::vector<Tool*> Tools;
  Tools tools;
public:
  ScreenBuffer(const Rect& rect);
  ~ScreenBuffer();

  // Mark an region as dirty in canvas space, not screen space
  void mark_dirty(int x, int y, int w, int h);
  void mark_dirty(const Rect& region);
  void force_full_refresh();
  
  void draw(SDL_Surface* target);

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
