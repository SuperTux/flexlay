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

#ifndef HEADER_WIDGET_HPP
#define HEADER_WIDGET_HPP

#include "SDL.h"
#include "math/rect.hpp"
#include "events.hpp"

/** */
class Widget
{
private:
  Rect  rect;
  bool  dirty;

public:
  Widget(const Rect& rect);
  virtual ~Widget();

  bool is_dirty() { return dirty; }
  void set_dirty(bool d) { dirty = d; }

  const Rect& get_rect() { return rect; }

  virtual bool do_update() { return true; }
  
  virtual void on_mouse_motion(const MouseMotionEvent& motion) =0;
  virtual void on_mouse_button(const MouseButtonEvent& button) =0;

  virtual void on_pen_motion(const PenEvent& button) {}

  virtual void on_enter() = 0;
  virtual void on_leave() = 0;

  virtual void draw(SDL_Surface* target) =0;

private:
  Widget (const Widget&);
  Widget& operator= (const Widget&);
};

#endif

/* EOF */
