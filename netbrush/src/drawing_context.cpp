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

#include <png.h>
#include <iostream>
#include "globals.hpp"
#include "screen_buffer.hpp"
#include "drawing_parameter.hpp"
#include "video.hpp"
#include "grayscale_buffer.hpp"
#include "stroke_buffer.hpp"
#include "navigation.hpp"
#include "drawing_context.hpp"

DrawingContext::DrawingContext(int w, int h) 
{
  drawable = create_surface(w, h);
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

  clip_to(rect, Rect(0, 0, get_width(), get_height()));

  stroke_buffer->clear(rect);
  stroke_buffer->set_param(param);
  stroke_buffer->draw_stroke(stroke, param);
  stroke_buffer->draw(drawable, rect, 0, 0);

  screen_buffer->mark_dirty(rect);

  // FIXME: Limit this to what changed on the buffer instead of whole screen
  navigation->update();
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

  screen_buffer->mark_dirty(Rect(Point(0,0), Size(rect.w, rect.h)));
}

void
DrawingContext::draw(SDL_Surface* target_surf, const Rect& rect, int x_of, int y_of)
{
  // rect is in screenspace, x_of, y_of tell how to go from canvas to screenspace

  if (1) // no zoom
    {
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
      SDL_BlitSurface(drawable, &source_pos, target_surf, &target_pos);
    }
  else
    {
      SDL_LockSurface(target_surf);
      SDL_LockSurface(drawable);

      Uint8* target = static_cast<Uint8*>(target_surf->pixels);
      Uint8* source = static_cast<Uint8*>(drawable->pixels);

      // FIXME: do clipping or do clipping at a higher level in the code
      float zoom = 2.0f;
      for(int y = rect.top; y < rect.bottom; ++y)
        for(int x = rect.left; x < rect.right; ++x)
          {
            int sx = int(x * zoom);
            int sy = int(y * zoom);

            target[y * target_surf->pitch + target_surf->format->BytesPerPixel * x + 2] = source[sy * drawable->pitch + drawable->format->BytesPerPixel * sx + 0];
            target[y * target_surf->pitch + target_surf->format->BytesPerPixel * x + 1] = source[sy * drawable->pitch + drawable->format->BytesPerPixel * sx + 1];
            target[y * target_surf->pitch + target_surf->format->BytesPerPixel * x + 0] = source[sy * drawable->pitch + drawable->format->BytesPerPixel * sx + 2];
          }

      SDL_UnlockSurface(drawable);
      SDL_UnlockSurface(target_surf);
    }
}

bool
DrawingContext::get_color(int x, int y, Color& color)
{
  if ((x >= 0 && x < drawable->w) &&
      (y >= 0 && y < drawable->h))
    {
      SDL_LockSurface(drawable);
      Uint8* source = static_cast<Uint8*>(drawable->pixels);

      SDL_GetRGB(*((Uint32*)(source+(y * drawable->pitch + drawable->format->BytesPerPixel * x))),
                 drawable->format,
                 &color.r,
                 &color.g,
                 &color.b);

  
      SDL_UnlockSurface(drawable);
      return true;
    }
  else
    {
      return false;
    }
}

void
DrawingContext::save_png(const std::string& filename)
{
  SDL_LockSurface(drawable);

  // FIXME: could/should check the right drawable format

  FILE* fp;
  fp = fopen(filename.c_str (), "wb");
  if (fp == NULL)
    {
      std::cout << "Error: Couldn't write " << filename << std::endl;
      return;
    }

  png_structp png_ptr;
  png_infop info_ptr;

  png_ptr  = png_create_write_struct(PNG_LIBPNG_VER_STRING, NULL, NULL, NULL);
  info_ptr = png_create_info_struct(png_ptr);

  png_init_io(png_ptr, fp);

  png_set_IHDR(png_ptr, info_ptr, 
               drawable->w, drawable->h, 8 /* bitdepth */,
               PNG_COLOR_TYPE_RGB,
               PNG_INTERLACE_NONE, 
               PNG_COMPRESSION_TYPE_BASE, 
               PNG_FILTER_TYPE_BASE);

  png_write_info(png_ptr, info_ptr);

  png_uint_32 height    = drawable->h;
  png_uint_32 row_bytes = drawable->w * 3;

  png_byte* image = new png_byte[height * row_bytes];
  png_bytep* row_pointers = new png_bytep[height];

  // fill the image with data
  for (int i = 0; i < drawable->w*drawable->h*3; ++i)
    image[i] = static_cast<unsigned char*>(drawable->pixels)[i];

  // generate row pointers
  for (unsigned int k = 0; k < height; k++)
    row_pointers[k] = image + (k * row_bytes);

  png_write_image(png_ptr, row_pointers);

  png_write_end(png_ptr, info_ptr);

  delete image;
  delete row_pointers;

  fclose(fp);
  SDL_UnlockSurface(drawable);
}

SDL_Surface*
DrawingContext::get_surface(const Rect& rect)
{
  SDL_Surface* region = create_surface(rect.get_width(), rect.get_height());
  // Fill with white (FIXME: should be alpha or background color)
  SDL_FillRect(region, NULL, SDL_MapRGB(region->format, 255, 255, 255));
  
  SDL_Rect source_pos;
  // FIXME: Do we need to clip those?
  source_pos.x = rect.left;
  source_pos.y = rect.top;
  source_pos.w = rect.get_width();
  source_pos.h = rect.get_height();

  SDL_Rect target_pos;
  target_pos.x = 0;
  target_pos.y = 0;

  SDL_BlitSurface(drawable, &source_pos, region, &target_pos);  

  return region;
}

void
DrawingContext::blit(SDL_Surface* source, const Point& pos)
{
  SDL_Rect target_pos;
  target_pos.x = pos.x;
  target_pos.y = pos.y;

  SDL_BlitSurface(source, NULL, drawable, &target_pos);

  screen_buffer->mark_dirty(Rect(pos, Size(source->w, source->h)));
}

/* EOF */
