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
#include "widget/widget.hpp"

/** */
class ScreenBuffer : public Widget
{
private:
  //SDL_Surface* buffer;
  Rect dirty_region;

  int scroll_offset_x;
  int scroll_offset_y;

  int old_scroll_offset_x;
  int old_scroll_offset_y;

  int click_pos_x;
  int click_pos_y;

  bool scrolling;

public:
  ScreenBuffer(const Rect& rect);
  ~ScreenBuffer();

  // Mark an region as dirty in canvas space, not screen space
  void mark_dirty(int x, int y, int w, int h);
  void mark_dirty(const Rect& region);
  
  void draw(SDL_Surface* target);

  void on_mouse_motion(const MouseMotionEvent& motion);
  void on_mouse_button(const MouseButtonEvent& button);
  
  void on_enter() {}
  void on_leave() {}

   bool do_update() { return false; }

private:
  ScreenBuffer (const ScreenBuffer&);
  ScreenBuffer& operator= (const ScreenBuffer&);
};

#endif

/* EOF */
