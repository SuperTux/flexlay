//  $Id: collision_mask.cxx,v 1.10 2003/09/04 22:51:04 grumbel Exp $
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

const unsigned int CollisionMask::int_width = 32;

void put_int(cm_uint32 val)
{
  for (unsigned int i = 0; i < CollisionMask::int_width; ++i)
    std::cout << ((val & (1 << (CollisionMask::int_width - 1 - i))) ? 'X' : '.');
  std::cout << std::endl;
}

CollisionMask::CollisionMask(const CollisionMask& mask)
  : width(mask.width),
    height(mask.height),
    pitch(mask.pitch)
{
  int len = height * pitch;
  data = new cm_uint32[len];
  memcpy(data, mask.data, sizeof(cm_uint32));
}

CollisionMask::CollisionMask(int arg_width, int arg_height)
  : width(arg_width),
    height(arg_height),
    pitch(width/int_width + (width % int_width == 0 ? 0 : 1))
{
  int len = height * pitch;

  data = new cm_uint32[len];
  memset(data, ~0, sizeof(cm_uint32) * len);
}

CollisionMask::CollisionMask(CL_PixelBuffer* provider)
{
  create_from(provider);
}

CollisionMask::CollisionMask(int arg_width, int arg_height, cm_uint32* arg_data)
  : width(arg_width), 
    height(arg_height),
    pitch(width/int_width + (width%int_width == 0 ? 0 : 1))
{
  int len = height * pitch;
  data = new cm_uint32[len];
  memcpy(data, arg_data, sizeof(cm_uint32));
}

CollisionMask::CollisionMask(const std::string filename)
{
  CL_PixelBuffer* provider = CL_ProviderFactory::load(filename);
  create_from(provider);
  delete provider;
}

void
CollisionMask::create_from(CL_PixelBuffer* provider)
{  
  provider->lock();

  width  = provider->get_width();
  height = provider->get_height();
  pitch  = (width/int_width + (width%int_width == 0 ? 0 : 1));

  int len = height * pitch;
  data = new cm_uint32[len];
  memset(data, 0, sizeof(cm_uint32) * len);

  for (int y = 0; y < height; ++y)
    for (int x = 0; x < width; ++x)
      {
        if (static_cast<unsigned char*>(provider->get_data())[(y * width + x) * 4] > 50)
          put_pixel(x, y, true);
      }

  std::cout << width << "x" << height << std::endl;

  provider->unlock(); 
}

CollisionMask::~CollisionMask()
{
  
}

void
CollisionMask::put_pixel(int x, int y, bool pixel)
{
  assert(x < width && y < height);

  if (pixel)
    data[y * pitch + x/int_width] |= (1 << (int_width - 1 - (x % int_width)));
  else
    data[y * pitch + x/int_width] &= (~(1 << (x % int_width)));
}

bool
CollisionMask::get_pixel(int x, int y) const
{
  assert(x >=0 && y >=0 && x < width && y < height);

  return data[y * pitch + x/int_width] & (1 << (int_width - 1 - (x % int_width)));
}

bool
CollisionMask::collides_with(const CollisionMask& mask, int x_of, int y_of, float scale_x, float scale_y) const
{
  int start_y = std::max(0, y_of); 
  int end_y   = std::min(height, static_cast<int>(y_of + (mask.height*scale_y) + 1));

  int start_x = std::max(0, x_of);
  int end_x   = std::min(width, static_cast<int>(x_of + (mask.width*scale_x) + 1));

  int t_x;
  int t_y;

  if (x_of < 0)
    t_x = int(-x_of / scale_x);
  else
    t_x = start_x - x_of;

  if (y_of < 0)
    t_y = int(-y_of / scale_y);
  else
    t_y = start_y - y_of;

  for (int x = start_x; x < end_x; ++x)
    for (int y = start_y; y < end_y; ++y)
      {
        if (get_pixel(x, y) 
            && mask.get_pixel(std::max(0, std::min(mask.width-1,  t_x + int((x - start_x) / scale_x))),
                              std::max(0, std::min(mask.height-1, t_y + int((y - start_y) / scale_y)))))
          return true;
      }

  return false; 
}

bool
CollisionMask::collides_with(const CollisionMask& mask, int pixel_x_of, int pixel_y_of) const
{
  if (bbox_collides_with(mask, pixel_x_of, pixel_y_of))
    {
      // Swap mask and *this so that pixel_x_of is always positiv
      if (pixel_x_of < 0)
        return mask.pixel_collides_with(*this, -pixel_x_of, -pixel_y_of);
      else
        return pixel_collides_with(mask, pixel_x_of, pixel_y_of);
    }
  return false;
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
  assert(pixel_x_of >= 0);

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
              cm_uint32 source = get_line(x, y);
              cm_uint32 target = mask.get_line(x - tile_x_of,
                                               y - pixel_y_of);
              if (source & target)
                return true;
            }
        }
    }
  else
    {
      int start_x = std::max(0, tile_x_of);
      int end_x   = std::min(pitch, mask.pitch + tile_x_of);

      unsigned int right_shift  = ((pixel_x_of % int_width) + int_width) % int_width;
      unsigned int left_shift   = int_width - right_shift;

      assert(left_shift >= 0 && left_shift < int_width);
      
      // pixel_x_of is always larger 0, so we can do a right-shift
      // Source: ######## ######## ########
      //              \.
      // Target: ...######## ########
      for (int y = start_y; y < end_y; ++y)
        {
          cm_uint32 source = get_line(start_x, y);
          cm_uint32 target = mask.get_line(start_x - tile_x_of,
                                           y - pixel_y_of);
          if (source & (target >> right_shift))
            return true;
        }

      // Source: ######## ######## ########
      //                  ./.   \. ./    \.
      // Target: ...######## ######## ########
      for (int x = start_x + 1; x < end_x; ++x)
        for (int y = start_y; y < end_y; ++y)
          {
            cm_uint32 source = get_line(x, y);
            cm_uint32 target1 = mask.get_line(x - tile_x_of - 1,
                                              y - pixel_y_of);
            cm_uint32 target2 = mask.get_line(x - tile_x_of,
                                              y - pixel_y_of);

            if (source & ((target1 << left_shift) |  (target2 >> right_shift)))
              return true;
          }

      // Source: ######## ######## ########
      //                           ./
      // Target: ...######## ######## 
      if (end_x < pitch && end_x - tile_x_of == mask.pitch)
        {
          for (int y = start_y; y < end_y; ++y)
            {
              cm_uint32 source  = get_line(end_x, y);
              cm_uint32 target = mask.get_line(end_x - 1 - tile_x_of,
                                               y - pixel_y_of);
            
              if (source & (target << left_shift))
                return true;              
            }
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
