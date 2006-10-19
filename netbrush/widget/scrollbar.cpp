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

#include "scrollbar.hpp"

Scrollbar::Scrollbar(int min_, int max_, int page_step_, Orientation orientation_, const Rect& rect)
  : Widget(rect),
    min(min_),
    max(max_),
    page_step(page_step_),
    orientation(orientation_),
    pos(min_ + (max_ - min_)/2)
{
  
}
  
void
Scrollbar::on_mouse_motion(const MouseMotionEvent& motion)
{ 
}

void
Scrollbar::on_mouse_button(const MouseButtonEvent& button)
{
}

void
Scrollbar::draw(SDL_Surface* target)
{
  SDL_Rect r;
  r.x = get_rect().left;
  r.y = get_rect().top;
  r.w = get_rect().get_width();
  r.h = get_rect().get_height();
  SDL_FillRect(target, &r, SDL_MapRGB(target->format, 200, 200, 200));

  if (orientation == HORIZONTAL)
    {
      r.x = get_rect().left + 2 + ((pos-min) * (get_rect().get_width()-4) / (max - min));
      r.y = get_rect().top + 2;
      r.w = page_step * (get_rect().get_width()-4) / (max - min);
      r.h = get_rect().get_height()-4;

      SDL_FillRect(target, &r, SDL_MapRGB(target->format, 0, 0, 0));
    }
  else // VERSION
    {
      r.x = get_rect().left + 2;
      r.y = get_rect().top  + 2 + ((pos-min) * (get_rect().get_height()-4) / (max - min));
      r.w = get_rect().get_width()-4;
      r.h = page_step * (get_rect().get_height()-4) / (max - min);

      SDL_FillRect(target, &r, SDL_MapRGB(target->format, 0, 0, 0));
    }
}

void
Scrollbar::set_pos(int p)
{
  pos = p;
  set_dirty(true);
}

/* EOF */
