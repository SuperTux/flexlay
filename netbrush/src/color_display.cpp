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
#include "color_display.hpp"

ColorDisplay::ColorDisplay(const Rect& rect)
  : Widget(rect), m_color(255, 0, 255)
{
}

void
ColorDisplay::draw(SDL_Surface* target)
{
  SDL_Rect r;
  r.x = get_rect().left;
  r.y = get_rect().top;
  r.w = get_rect().get_width();
  r.h = get_rect().get_height();

  SDL_FillRect(target, &r, SDL_MapRGB(target->format, m_color.r, m_color.g, m_color.b));
  if (0) std::cout << "Color: " << (int)m_color.r << " " << (int)m_color.g << " " << (int)m_color.b << std::endl;
}

void
ColorDisplay::set_color(const Color& color)
{
  m_color = color;
  set_dirty(true);
}

/* EOF */
