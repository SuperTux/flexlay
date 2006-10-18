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
#include "screen_buffer.hpp"
#include "drawing_parameter.hpp"
#include "video.hpp"
#include "grayscale_buffer.hpp"
#include "stroke_buffer.hpp"
#include "drawing_context.hpp"

DrawingContext::DrawingContext(int w, int h) 
{
  Uint32 rmask, gmask, bmask, amask;

  /* SDL interprets each pixel as a 32-bit number, so our masks must depend
     on the endianness (byte order) of the machine */
#if SDL_BYTEORDER == SDL_BIG_ENDIAN
  rmask = 0xff000000;
  gmask = 0x00ff0000;
  bmask = 0x0000ff00;
  amask = 0; //0x000000ff;
#else
  rmask = 0x000000ff;
  gmask = 0x0000ff00;
  bmask = 0x00ff0000;
  amask = 0; //0xff000000;
#endif

  drawable = SDL_CreateRGBSurface(SDL_SWSURFACE, w, h, 24,
                                  rmask, gmask, bmask, amask);
  if(drawable == NULL) {
    fprintf(stderr, "CreateRGBSurface failed: %s\n", SDL_GetError());
    exit(1);
  }

  stroke_buffer = new StrokeBuffer(w, h);
}

DrawingContext::~DrawingContext() 
{
  SDL_FreeSurface(drawable);
}

void
DrawingContext::draw_stroke(const Stroke& stroke, DrawingParameter* param)
{
  Rect rect = stroke.get_bounding_rect();

  rect.left -= param->thickness()/2;
  rect.top  -= param->thickness()/2;
  
  rect.right  += param->thickness()/2;
  rect.bottom += param->thickness()/2;

  stroke_buffer->clear(rect);
  stroke_buffer->set_param(param);
  stroke_buffer->draw_stroke(stroke, param);
  stroke_buffer->draw(drawable, rect, 0, 0);

  if (0)
    {
      SDL_Surface* brush = param->get_brush_surface();
  
      Stroke::Dabs dabs = stroke.get_interpolated_dabs(param->spacing, param->spacing);
      for(Stroke::Dabs::iterator i = dabs.begin(); i != dabs.end(); ++i)
        {
          SDL_Rect rect;
          rect.x = int(i->pos.x)-(brush->w/2);
          rect.y = int(i->pos.y)-(brush->h/2);
          rect.w = brush->w;
          rect.h = brush->h;
                  
          SDL_BlitSurface(brush, 0, drawable, &rect);
        }
    }

  screen_buffer->mark_dirty(rect);
}

void
DrawingContext::clear()
{
  SDL_Rect rect;
  
  rect.x = 0;
  rect.y = 0;
  rect.w = drawable->w;
  rect.h = drawable->h;

  SDL_FillRect(drawable, &rect, SDL_MapRGB(drawable->format, 255, 255, 255));

  screen_buffer->mark_dirty(&rect);
}

void
DrawingContext::draw(SDL_Surface* target, const Rect& rect, int x_of, int y_of)
{
  // rect is in screenspace, x_of, y_of tell how to go from canvas to screenspace

  SDL_Rect target_pos;
  target_pos.x = rect.left;
  target_pos.y = rect.top;
  target_pos.w = rect.get_width();
  target_pos.h = rect.get_height();

  SDL_Rect source_pos;
  source_pos.x = rect.left - x_of;
  source_pos.y = rect.top  - y_of;
  source_pos.w = rect.get_width();
  source_pos.h = rect.get_height();

  SDL_Rect r;
  r.x = 0;
  r.y = 0;
  r.w = drawable->w;
  r.h = drawable->h;

  clip_to(&source_pos, &r);
  SDL_BlitSurface(drawable, &source_pos, target, &target_pos);
}

/* EOF */
