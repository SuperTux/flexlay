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
  up_surface    = IMG_Load(DATADIR "/icons/up.png");
  down_surface  = IMG_Load(DATADIR "/icons/down.png");
  hover_surface = IMG_Load(DATADIR "/icons/hover.png");
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
Button::draw(GraphicContext& gc)
{
  switch(state)
    {
    case UP:
      if (hover)
        gc.blit(hover_surface, Point(0, 0));
      else
        gc.blit(up_surface, Point(0, 0));
      break;

    case DOWN:
      gc.blit(up_surface, Point(0, 0));
      break;
    }

  gc.blit(icon, Point(6, 6));
}

/* EOF */
