/*            _   ___              _   
**   _ _  ___| |_| _ )_ _ _  _ _ _| |_ 
**  | ' \/ -_)  _| _ \ '_| || (_-<|   |
**  |_||_\___|\__|___/_|  \_,_/__/|_|_|
**  netBrush - Copyright (C) 2006 Ingo Ruhnke <grumbel@gmx.de>
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
                        "\x7f                                "
                        "!\"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ"
                        "[\\]^_`abcdefghijklmnopqrstuvwxyz{|}~");
  tty = TTY_Create(rect.get_width()/8, rect.get_height()/8+1, font);
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
TextView::draw(GraphicContext& gc)
{
  if (1)
    gc.fill_rect(Rect(0, 0, get_rect().get_width(), get_rect().get_height()),
                 Color(64, 64, 64));
  
  // FIXME: move font handling in GraphicContext
  TTY_Blit(tty, gc.get_surface(), get_rect().left, get_rect().top);
}

void
TextView::put(const std::string& str)
{
  TTY_write(tty, str.c_str(), str.size());
  set_dirty(true);
}

/* EOF */
