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

#include <algorithm>
#include "globals.hpp"
#include "widget_manager.hpp"
#include "slider_widget.hpp"

SliderWidget::SliderWidget(const Rect& rect_, SliderCallback* callback_)
  : Widget(rect_), callback(callback_), pos(0.5f), dragging(false)
{
}

SliderWidget::~SliderWidget()
{
  delete callback;
}

void
SliderWidget::on_mouse_motion(const MouseMotionEvent& motion)
{
  set_dirty(true);
  if (dragging)
    {
      pos = float(motion.x) / get_rect().get_width();
      pos = std::max(0.0f, std::min(pos, 1.0f));
      (*callback)(pos);
    }

}

void
SliderWidget::on_mouse_button(const MouseButtonEvent& button)
{
  if (button.button == 1)
    {
      if (button.state == SDL_RELEASED)
        {
          dragging = false;
          widget_manager->ungrab(this);
        }
      else if (button.state == SDL_PRESSED)
        {
          dragging = true;

          pos = float(button.x) / get_rect().get_width();
          pos = std::max(0.0f, std::min(pos, 1.0f));

          (*callback)(pos);

          set_dirty(true);
          widget_manager->grab(this);
        }
    }
}

void
SliderWidget::on_enter()
{
}

void
SliderWidget::on_leave()
{
}

void
SliderWidget::draw(SDL_Surface* target)
{
  SDL_Rect rect;
  rect.x = get_rect().left;
  rect.y = get_rect().top;
  rect.w = get_rect().get_width();
  rect.h = get_rect().get_height();

  SDL_FillRect(target, &rect, SDL_MapRGB(target->format, 200, 200, 200));

  rect.x += 2;
  rect.y += 10;
  rect.w -= 4;
  rect.h -= 20;

  SDL_FillRect(target, &rect, SDL_MapRGB(target->format, 100, 100, 100));

  SDL_Rect slider;
  slider.x = int(get_rect().get_width() * pos) + get_rect().left - 4;
  slider.y = 2 + get_rect().top;
  slider.w = 8;
  slider.h = get_rect().get_height() - 4;

  SDL_FillRect(target, &slider, SDL_MapRGB(target->format, 0, 0, 0));
}

/* EOF */
