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
#include "video.hpp"
#include "globals.hpp"
#include "widget/widget_manager.hpp"
#include "color.hpp"
#include "controller.hpp"
#include "saturation_value_picker.hpp"

SaturationValuePicker::SaturationValuePicker(const Rect& rect_)
  : Widget(rect_), dragging(false), click_pos(64, 64)
{
  surface = create_surface(rect_.get_width(), rect_.get_height());
}

void
SaturationValuePicker::set_color(const Color& color_)
{
  color = color_;
  SDL_LockSurface(surface);

  Uint8* data = static_cast<Uint8*>(surface->pixels);
  
  for(int y = 0; y < surface->h; ++y)
    for(int x = 0; x < surface->w; ++x)
      {
        int value      = 255 - (255 * x / surface->w);
        int saturation = (255 * y / surface->h);
        
        data[3*(y * surface->w + x)+0] = (0*value + (255-value) * ((255 * saturation + color.r * (255 - saturation))/255))/255;
        data[3*(y * surface->w + x)+1] = (0*value + (255-value) * ((255 * saturation + color.g * (255 - saturation))/255))/255;
        data[3*(y * surface->w + x)+2] = (0*value + (255-value) * ((255 * saturation + color.b * (255 - saturation))/255))/255;
      }
  SDL_UnlockSurface(surface);
  
  set_dirty(true);
}

Color
SaturationValuePicker::get_color(Uint8 value, Uint8 saturation) const
{
  return Color((0*value + (255-value) * ((255 * saturation + color.r * (255 - saturation))/255))/255,
               (0*value + (255-value) * ((255 * saturation + color.g * (255 - saturation))/255))/255,
               (0*value + (255-value) * ((255 * saturation + color.b * (255 - saturation))/255))/255);  
}

void
SaturationValuePicker::on_mouse_motion(const MouseMotionEvent& motion)
{
  set_dirty(true);
  if (dragging)
    {
      click_pos.x = std::min(std::max(0, motion.x), get_rect().get_width());
      click_pos.y = std::min(std::max(0, motion.y), get_rect().get_height());
      controller->set_color_value_saturation(255 - 255 * click_pos.x/get_rect().get_width(),
                                             255 * click_pos.y/get_rect().get_height());
    }
}

void
SaturationValuePicker::on_mouse_button(const MouseButtonEvent& button)
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

          click_pos.x = std::min(std::max(0, button.x), get_rect().get_width());
          click_pos.y = std::min(std::max(0, button.y), get_rect().get_height());
      
          set_dirty(true);
          widget_manager->grab(this);
          controller->set_color_value_saturation(255 - 255 * click_pos.x/get_rect().get_width(),
                                                 255 * click_pos.y/get_rect().get_height());
        }
    }
}

void
SaturationValuePicker::draw(GraphicContext& gc)
{
  gc.blit(surface, Point(0, 0));
  gc.draw_line(Point(click_pos.x, 0),
               Point(click_pos.x, get_rect().get_height()),
               Color(0, 0, 0));
  gc.draw_line(Point(0, click_pos.y),
               Point(get_rect().get_width(), click_pos.y),
               Color(0, 0, 0));
}

/* EOF */
