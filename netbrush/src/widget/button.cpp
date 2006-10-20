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

#include <iostream>
#include "SDL_image.h"
#include "widget_manager.hpp"
#include "../globals.hpp"
#include "button.hpp"

Button::Button(SDL_Surface* icon_, const Rect& rect_, ButtonCallback* callback_)
  : Widget(rect_),
    callback(callback_),
    state(UP),
    hover(false),
    icon(icon_)
{
  up_surface    = IMG_Load("data/icons/up.png");
  down_surface  = IMG_Load("data/icons/down.png");
  hover_surface = IMG_Load("data/icons/hover.png");
}

Button::~Button()
{
  
}

void
Button::on_enter()
{
  hover = true;
  set_dirty(true);
}

void
Button::on_leave()
{
  hover = false;
  set_dirty(true);
}

void
Button::on_mouse_motion(const MouseMotionEvent& motion)
{
  // std::cout << "Motion: " << motion.x << " " << motion.y << std::endl;
  // FIXME: borked
}

void
Button::on_mouse_button(const MouseButtonEvent& button)
{
  if (button.button == 1 && button.state == SDL_PRESSED)
    {
      state = DOWN;
      set_dirty(true);
      widget_manager->grab(this);
      callback->on_press(this);
      
    }
  else if (button.button == 1 && button.state == SDL_RELEASED)
    {
      state = UP;
      widget_manager->ungrab(this);
      set_dirty(true);
      callback->on_release(this);
      // FIXME: Need to check if inside button rectangle
      callback->on_click(this);
    }
}

void
Button::draw(SDL_Surface* target)
{
  //Uint32 color = SDL_MapRGB(target->format, 255, 0, 0);

  SDL_Rect r;

  r.x = get_rect().left;
  r.y = get_rect().top;

  r.w = get_rect().get_width();
  r.h = get_rect().get_height();

  switch(state)
    {
    case UP:
      if (hover)
        SDL_BlitSurface(hover_surface, NULL, target, &r);
      else
        SDL_BlitSurface(up_surface, NULL, target, &r);
      break;

    case DOWN:
      SDL_BlitSurface(down_surface, NULL, target, &r);
      break;
    }

  r.x = get_rect().left + 6;
  r.y = get_rect().top  + 6;

  SDL_BlitSurface(icon, NULL, target, &r);

  //SDL_FillRect(target, get_rect(), color);
}

/* EOF */
