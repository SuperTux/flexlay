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
#include "math/rect.hpp"
#include "grayscale_buffer.hpp"
#include "stroke.hpp"
#include "video.hpp"
#include "drawing_parameter.hpp"
#include "stroke_buffer.hpp"

StrokeBuffer::StrokeBuffer(int w, int h)
  : param(0)
{
  buffer = new GrayscaleBuffer(w, h, 0);
  stroke = new Stroke();
}

StrokeBuffer::~StrokeBuffer()
{
  delete stroke;
  delete buffer;
}

void
StrokeBuffer::clear(const Rect& rect)
{
  buffer->fill_rect(rect, 0);  
}

void
StrokeBuffer::clear()
{
  buffer->clear(0);

  // FIXME: This doesn't belong here
  delete this->stroke;
  this->stroke = new Stroke();
}

void
StrokeBuffer::add_dab(const Dab& dab)
{
  //  for(int y = - 5; y < 10; ++y)
  //for(int x = - 5; x < 10; ++x)
  //buffer->at(int(dab.pos.x)+x, int(dab.pos.y)+y) = 128;

  GrayscaleBuffer* brush = param->get_brush_buffer();

  stroke->add_dab(dab);
  if (1)
    {
      buffer->blit(brush,
                   static_cast<int>(dab.pos.x - brush->get_width()/2), 
                   static_cast<int>(dab.pos.y - brush->get_height()/2), 
                   GrayscaleBuffer::ALPHA);
    }
  else
    { // FIXME: SLOW!
      buffer->clear(0);
      draw_stroke(*stroke, param);
    }
}

void
StrokeBuffer::draw_stroke(const Stroke& stroke, DrawingParameter* param)
{
  GrayscaleBuffer* brush = param->get_brush_buffer();

  //Stroke::Dabs dabs = stroke.get_interpolated_dabs(param->spacing, param->spacing);
  Stroke::Dabs dabs = stroke.get_dabs();
  for(Stroke::Dabs::iterator i = dabs.begin(); i != dabs.end(); ++i)
    {
      buffer->blit(brush,
                   static_cast<int>(i->pos.x - brush->get_width()/2), 
                   static_cast<int>(i->pos.y - brush->get_height()/2), 
                   GrayscaleBuffer::ALPHA);
    }
}

void
StrokeBuffer::draw(SDL_Surface* target, const Rect& rect, int x_of, int y_of)
{ 
  // rect is in screenspace, x_of, y_of tell how to go from canvas to screenspace
  SDL_LockSurface(target);

  Uint8* dst = static_cast<Uint8*>(target->pixels);
  Uint8* src = buffer->get_data();
  
  if (target == screen) // FIXME: Ugly workaround
    {
      for(int y = rect.top; y < rect.bottom; ++y)
        for(int x = rect.left; x < rect.right; ++x)
          {
            Uint8* d = dst + (y * target->pitch + target->format->BytesPerPixel * x);
            Uint8  s = src[(y - y_of) * buffer->get_width() + (x - x_of)];

            s = (s * param->opacity)/255;

            d[0] = ((255 - s) * d[0] + (s * param->color.b))/255;
            d[1] = ((255 - s) * d[1] + (s * param->color.g))/255;
            d[2] = ((255 - s) * d[2] + (s * param->color.r))/255;
          }
    }
  else
    {
      for(int y = rect.top; y < rect.bottom; ++y)
        for(int x = rect.left; x < rect.right; ++x)
          {
            Uint8* d = dst + (y * target->pitch + target->format->BytesPerPixel * x);
            Uint8  s = src[(y - y_of) * buffer->get_width() + (x - x_of)];

            s = (s * param->opacity)/255;

            d[0] = ((255 - s) * d[0] + (s * param->color.r))/255;
            d[1] = ((255 - s) * d[1] + (s * param->color.g))/255;
            d[2] = ((255 - s) * d[2] + (s * param->color.b))/255;
          }
    }

  SDL_UnlockSurface(target);
}

void
StrokeBuffer::set_param(DrawingParameter* param_)
{
  param = param_;
}

/* EOF */
