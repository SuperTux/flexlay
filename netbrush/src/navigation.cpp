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
#include "video.hpp"
#include "globals.hpp"
#include "drawing_context.hpp"
#include "screen_buffer.hpp"
#include "navigation.hpp"

Navigation::Navigation(const Rect& rect_)
  : Widget(rect_), dragging(false)
{
  surface = create_surface(rect_.get_width(), rect_.get_height());  
}

void
Navigation::on_mouse_motion(const MouseMotionEvent& motion)
{
  if (dragging)
    {
      screen_buffer->move_to(Point(draw_ctx->get_width()  * motion.x / get_rect().get_width(),
                                   draw_ctx->get_height() * motion.y / get_rect().get_height()));
    }
}

void
Navigation::on_mouse_button(const MouseButtonEvent& button)
{
  if (button.button == 1)
    {
      if (button.state == SDL_RELEASED)
        {
          screen_buffer->move_to(Point(draw_ctx->get_width()  * button.x / get_rect().get_width(),
                                       draw_ctx->get_height() * button.y / get_rect().get_height()));
          dragging = false;
        }
      else if (button.state == SDL_PRESSED)
        {
          screen_buffer->move_to(Point(draw_ctx->get_width()  * button.x / get_rect().get_width(),
                                       draw_ctx->get_height() * button.y / get_rect().get_height()));
          dragging = true;
        }
    }
}

void
Navigation::draw(GraphicContext& gc)
{
  gc.blit(surface, Point(0, 0));
}

void
Navigation::update()
{
  //std::cout << "Navigation::update" << std::endl;
  SDL_Surface* drawable = draw_ctx->get_surface();
  SDL_LockSurface(drawable);
  SDL_LockSurface(surface);

  Uint8* target = static_cast<Uint8*>(surface->pixels);
  Uint8* source = static_cast<Uint8*>(drawable->pixels);

  int sx = drawable->w / surface->w;
  int sy = drawable->h / surface->h;
  
  for(int y = 0; y < surface->h; ++y)
    for(int x = 0; x < surface->pitch; ++x)
      {
        target[y * surface->pitch + x] = source[(y*sy) * drawable->pitch + (x*sx)];
      }

  SDL_UnlockSurface(surface);
  SDL_UnlockSurface(drawable);

  set_dirty(true);
}

/* EOF */
