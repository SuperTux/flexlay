//  $Id$
//
//  Flexlay - A Generic 2D Game Editor
//  Copyright (C) 2002 Ingo Ruhnke <grumbel@gmx.de>
//
//  This program is free software; you can redistribute it and/or
//  modify it under the terms of the GNU General Public License
//  as published by the Free Software Foundation; either version 2
//  of the License, or (at your option) any later version.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//  GNU General Public License for more details.
//
//  You should have received a copy of the GNU General Public License
//  along with this program; if not, write to the Free Software
//  Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.

#include <assert.h>
#include <iostream>
#include <algorithm>
#include <ClanLib/Display/pixel_format.h>
#include <ClanLib/Display/palette.h>
#include "blitter.hxx"

void 
blit(CL_PixelBuffer target, CL_PixelBuffer brush, int x_pos, int y_pos)
{
  target.lock();
  brush.lock();

  int start_x = std::max(0, -x_pos);
  int start_y = std::max(0, -y_pos);
  
  int end_x = std::min(brush.get_width(),  target.get_width()  - x_pos);
  int end_y = std::min(brush.get_height(), target.get_height() - y_pos);

  unsigned char* target_buf = static_cast<unsigned char*>(target.get_data());
  unsigned char* brush_buf  = static_cast<unsigned char*>(brush.get_data());

  int target_width = target.get_width();
  int brush_width  = brush.get_width();

  if (brush.get_format().get_type() == pixelformat_rgba)
    {
      if (brush.get_format().get_depth() == 32)
        {
          for (int y = start_y; y < end_y; ++y)
            for (int x = start_x; x < end_x; ++x)
              {
                int target_pos = (y + y_pos) * target_width + x + x_pos;
                int brush_pos  = y * brush_width + x;

                unsigned char a  = brush_buf[4*brush_pos + 0];
                unsigned char r  = brush_buf[4*brush_pos + 1];
                unsigned char g  = brush_buf[4*brush_pos + 2];
                unsigned char b  = brush_buf[4*brush_pos + 3];

                unsigned char ta = target_buf[4*target_pos + 0];
                unsigned char tr = target_buf[4*target_pos + 1];
                unsigned char tg = target_buf[4*target_pos + 2];
                unsigned char tb = target_buf[4*target_pos + 3];

                float alpha  = a/255.0f;
        
                target_buf[4*target_pos + 0] = std::min(255, ta + a);
                target_buf[4*target_pos + 1] = std::min(255, int((1-alpha)*tr + alpha*r));
                target_buf[4*target_pos + 2] = std::min(255, int((1-alpha)*tg + alpha*g));
                target_buf[4*target_pos + 3] = std::min(255, int((1-alpha)*tb + alpha*b));
              }
        }
      else if (brush.get_format().get_depth() == 24)
        {
          for (int y = start_y; y < end_y; ++y)
            for (int x = start_x; x < end_x; ++x)
              {
                int target_pos = (y + y_pos) * target_width + x + x_pos;
                int brush_pos  = y * brush_width + x;

                target_buf[4*target_pos + 0] = 255;
                target_buf[4*target_pos + 1] = brush_buf[3*brush_pos + 0];
                target_buf[4*target_pos + 2] = brush_buf[3*brush_pos + 1];
                target_buf[4*target_pos + 3] = brush_buf[3*brush_pos + 2];
              }
        }
      else
        {
          std::cout << "Unsupported bpp: " << brush.get_format().get_depth() << std::endl;
        }
    }
  else if (brush.get_format().get_type() == pixelformat_index)
    {
      CL_Palette palette = brush.get_palette();
      for (int y = start_y; y < end_y; ++y)
        for (int x = start_x; x < end_x; ++x)
          {
            int target_pos = (y + y_pos) * target_width + x + x_pos;
            int brush_pos  = y * brush_width + x;
            
            target_buf[4*target_pos + 0] = 255;
            target_buf[4*target_pos + 1] = palette.colors[brush_buf[brush_pos]].get_blue();
            target_buf[4*target_pos + 2] = palette.colors[brush_buf[brush_pos]].get_green();
            target_buf[4*target_pos + 3] = palette.colors[brush_buf[brush_pos]].get_red();
          }
    }
  else
    {
      assert(!"Unknown pixelformat type");
    }
    


  brush.unlock();
  target.unlock();
}

void clear(CL_PixelBuffer canvas)
{
  unsigned char* buffer;

  canvas.lock();
  buffer = static_cast<unsigned char*>(canvas.get_data());
  memset(buffer, 0, sizeof(unsigned char) * canvas.get_pitch() * canvas.get_height());
  canvas.unlock();
}

/* EOF */
