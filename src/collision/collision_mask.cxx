//  $Id: collision_mask.cxx,v 1.2 2003/09/01 20:56:48 grumbel Exp $
//
//  Pingus - A free Lemmings clone
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

#include <ClanLib/Display/Providers/provider_factory.h>
#include <ClanLib/Display/pixel_buffer.h>
#include <ClanLib/Display/graphic_context.h>
#include <ClanLib/Display/color.h>
#include <assert.h>
#include <iostream>
#include <algorithm>
#include <string.h>
#include "collision_mask.hxx"

const int CollisionMask::int_width = 32;

void put_int(cl_uint32 val)
{
  for (int i = 0; i < 32; ++i)
    std::cout << ((val & (1 << (31-i))) ? 'X' : '.');
  std::cout << std::endl;
}

CollisionMask::CollisionMask(int arg_width, int arg_height)
  : width(arg_width),
    height(arg_height),
    pitch(width/int_width + (width % int_width == 0 ? 0 : 1))
{
  int len = height * pitch;

  data = new cl_uint32[len];
  memset(data, ~0, sizeof(cl_uint32) * len);
}

CollisionMask::CollisionMask(int arg_width, int arg_height, cl_uint32* arg_data)
  : width(arg_width), 
    height(arg_height),
    pitch(width/int_width + (width%int_width == 0 ? 0 : 1))
{
  int len = height * pitch;
  data = new cl_uint32[len];
  memset(data, 0, sizeof(cl_uint32) * len);
  memcpy(data, arg_data, sizeof(cl_uint32));
}

CollisionMask::CollisionMask(const std::string filename)
{
  CL_PixelBuffer* provider = CL_ProviderFactory::load(filename);
  
  provider->lock();

  width  = provider->get_width();
  height = provider->get_height();
  pitch  = (width/int_width + (width%int_width == 0 ? 0 : 1));

  int len = height * pitch;
  data = new cl_uint32[len];
  memset(data, 0, sizeof(cl_uint32) * len);

  for (int y = 0; y < height; ++y)
    for (int x = 0; x < width; ++x)
      {
        if (static_cast<unsigned char*>(provider->get_data())[(y * width + x) * 4] > 50)
          put_pixel(x, y, true);
      }

  std::cout << width << "x" << height << std::endl;

  provider->unlock();

  delete provider;
}

CollisionMask::~CollisionMask()
{
  
}

void
CollisionMask::put_pixel(int x, int y, bool pixel)
{
  assert(x < width && y < height);

  if (pixel)
    data[y * pitch + x/32] |= (1 << (32 - (x % 32)));
  else
    data[y * pitch + x/32] &= (~(1 << (x % 32)));
}

bool
CollisionMask::get_pixel(int x, int y) const
{
  assert(x < width && y < height);

  return data[y * pitch + x/32] & (1 << (32 - (x % 32)));
}

cl_uint32
CollisionMask::get_line(int x, int y) const
{
  assert(x < pitch && y < height);

  return data[y * pitch + x];
}

bool
CollisionMask::collides_with(const CollisionMask& mask, int pixel_x_of, int pixel_y_of) const
{
  return bbox_collides_with(mask, pixel_x_of, pixel_y_of)
    && pixel_collides_with(mask, pixel_x_of, pixel_y_of);
}

bool
CollisionMask::slow_pixel_collides_with(const CollisionMask& mask, int pixel_x_of, int pixel_y_of) const
{
  int start_y = std::max(0, pixel_y_of); 
  int end_y   = std::min(height, mask.height + pixel_y_of);

  int start_x = std::max(0, pixel_x_of);
  int end_x   = std::min(width, mask.width + pixel_x_of);

  for (int x = start_x; x < end_x; ++x)
    for (int y = start_y; y < end_y; ++y)
      {
        if (get_pixel(x, y) && mask.get_pixel(x - pixel_x_of, y - pixel_y_of))
          return true;
      }

  return false;
}

bool
CollisionMask::pixel_collides_with(const CollisionMask& mask, int pixel_x_of, int pixel_y_of) const
{
  // All coordinates are relative to the 'this' mask
  int tile_x_of = pixel_x_of / int_width;

  int start_y = std::max(0, pixel_y_of); 
  int end_y   = std::min(height, mask.height + pixel_y_of);

  // We can directly compare ints vs ints, no shifting required
  if (pixel_x_of % int_width == 0)
    {
      int start_x = std::max(0, tile_x_of);
      int end_x   = std::min(pitch, mask.pitch + tile_x_of);

      for (int x = start_x; x < end_x; ++x)
        {
          for (int y = start_y; y < end_y; ++y)
            {
              cl_int32 source = get_line(x, y);
              cl_int32 target = mask.get_line(x - tile_x_of,
                                              y - pixel_y_of);
              if (source & target)
                return true;
            }
        }
    }
  else
    {
      int start_x = std::max(0, tile_x_of);
      int end_x   = std::min(pitch, mask.pitch + tile_x_of + 1);

      int left_shift  = ((32 * 1000) + pixel_x_of) % 32;
      int right_shift = 32 - left_shift;

      if (0)
      std::cout << "start: " << start_x << " end: " << end_x
                << " pitch: " << pitch << " height: " << height 
                << " mpitch: " << mask.pitch << " mheight: " << mask.height 
                << std::endl;

      for (int y = start_y; y < end_y; ++y)
        {
          cl_int32 source = get_line(start_x, y);
          cl_int32 target = mask.get_line(start_x - tile_x_of,
                                          y - pixel_y_of);
          if (pixel_x_of < 0)
            target = target << right_shift;
          else
            target = target >> left_shift;
              
          if (source & target)
            return true;
        }

    }

  return false;
}

bool
CollisionMask::bbox_collides_with(const CollisionMask& mask, int x_of, int y_of) const
{
  return !(x_of > width 
           ||   y_of > height
           ||   x_of + mask.width  < 0 
           ||   y_of + mask.height < 0);
}

/* EOF */
