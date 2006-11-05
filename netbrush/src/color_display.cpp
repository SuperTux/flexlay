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
#include "globals.hpp"
#include "controller.hpp"
#include "color_display.hpp"

ColorDisplay::ColorDisplay(const Rect& rect)
  : Widget(rect), 
    foreground(255, 0, 255),
    background(100, 100, 0)
{
}

void
ColorDisplay::on_mouse_button(const MouseButtonEvent& button) 
{
  if (button.state == SDL_PRESSED)
    {
      std::swap(background, foreground);
      controller->set_color(foreground);
      set_dirty(true);
      controller->puts("Colors swapped");
    }
}

void
ColorDisplay::draw(GraphicContext& gc)
{
  gc.fill_rect(Rect(Point(get_rect().get_width()/3,
                          get_rect().get_height()/3),
                    Size(2*get_rect().get_width()/3,
                         2*get_rect().get_height()/3)),
               Color(128, 128, 128));

  gc.fill_rect(Rect(Point(get_rect().get_width()/3+1,
                          get_rect().get_height()/3+1),
                    Size(2*get_rect().get_width()/3-2,
                         2*get_rect().get_height()/3-2)),
               Color(background.r, background.g, background.b));

  gc.fill_rect(Rect(Point(0, 0),
                    Size(2*get_rect().get_width()/3,
                         2*get_rect().get_height()/3)),
               Color(128, 128, 128));
                 
  gc.fill_rect(Rect(Point(1, 1),
                    Size(2*get_rect().get_width()/3-2,
                         2*get_rect().get_height()/3-2)),
               Color(foreground.r, foreground.g, foreground.b));

  if (0)
    std::cout << "Color: " << (int)foreground.r << " " << (int)foreground.g << " " << (int)foreground.b << std::endl;
}

void
ColorDisplay::set_color(const Color& color)
{
  foreground = color;
  set_dirty(true);
}

/* EOF */
