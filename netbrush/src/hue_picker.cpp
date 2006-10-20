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

#include <vector>
#include "video.hpp"
#include "color.hpp"
#include "globals.hpp"
#include "widget/widget_manager.hpp"
#include "controller.hpp"
#include "globals.hpp"

#include "hue_picker.hpp"

HuePicker::HuePicker(const Rect& rect_)
  : Widget(rect_), dragging(0)
{
  surface = create_surface(rect_.get_width(), rect_.get_height());
  
  SDL_LockSurface(surface);
  Uint8* data = static_cast<Uint8*>(surface->pixels);

  for(int y = 0; y < surface->h; ++y)
    for(int x = 0; x < surface->w; ++x)
      {
        int hue = 255 * x / surface->w;
        
        const Color& color = Color::from_hue(hue);
        data[3*(y * surface->w + x)+0] = color.r;
        data[3*(y * surface->w + x)+1] = color.g;
        data[3*(y * surface->w + x)+2] = color.b;
      }
  SDL_UnlockSurface(surface);  
}

void
HuePicker::on_mouse_motion(const MouseMotionEvent& motion)
{
  if (dragging)
    {
      click_pos.x = motion.x;
      click_pos.y = motion.y;
      controller->set_color_hue(255 * click_pos.x / get_rect().get_width());
      set_dirty(true);
    }
}

void
HuePicker::on_mouse_button(const MouseButtonEvent& button)
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

          click_pos.x = button.x;
          click_pos.y = button.y;

          controller->set_color_hue(255 * click_pos.x / get_rect().get_width());

          set_dirty(true);
          widget_manager->grab(this);
        }
    }
}

void
HuePicker::draw(SDL_Surface* target)
{
  SDL_Rect pos;
  pos.x = get_rect().left;
  pos.y = get_rect().top;

  SDL_Rect vline;
  vline.x = click_pos.x + get_rect().left;
  vline.y = 0 + get_rect().top;
  vline.w = 1;
  vline.h = get_rect().get_height();

  SDL_BlitSurface(surface, 0, target, &pos);
  SDL_FillRect(target, &vline, SDL_MapRGB(target->format, 0, 0, 0));
}

/* EOF */
