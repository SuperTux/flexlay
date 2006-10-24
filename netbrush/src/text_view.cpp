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
#include "SDL_tty.h"
#include "SDL_image.h"
#include "text_view.hpp"

TextView::TextView(const Rect& rect)
  : Widget(rect)
{
  SDL_Surface* temp = IMG_Load("data/fonts/8x8font.png");
  font = TTY_CreateFont(temp, 8, 8, 
                    "\x7f                                !\"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ"
                    "[\\]^_`abcdefghijklmnopqrstuvwxyz{|}~");
  tty = TTY_Create(rect.get_width()/8, rect.get_height()/8, font);
  TTY_Clear(tty);
  TTY_printf(tty, "netBrush Version 0.0.1");
}
 
TextView::~TextView()
{
  TTY_Free(tty);
  TTY_FreeFont(font);
}
 
void
TextView::on_mouse_motion(const MouseMotionEvent& motion)
{
  set_dirty(true);
}

void
TextView::on_mouse_button(const MouseButtonEvent& button)
{
}

void
TextView::draw(SDL_Surface* target)
{
  if (0)
    {
      SDL_Rect r;
      r.x = get_rect().left;
      r.y = get_rect().top;
      r.w = get_rect().get_width();
      r.h = get_rect().get_height();

      SDL_FillRect(target, &r, SDL_MapRGB(target->format, 255, 255, 255));
    }
  TTY_Blit(tty, target, get_rect().left, get_rect().top);
}

/* EOF */
