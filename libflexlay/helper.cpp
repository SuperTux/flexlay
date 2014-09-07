// Flexlay - A Generic 2D Game Editor
// Copyright (C) 2002 Ingo Ruhnke <grumbel@gmail.com>
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <http://www.gnu.org/licenses/>.

#include "helper.hpp"

#include <iostream>

#include "blitter.hpp"
#include "globals.hpp"

typedef std::map<std::string, PixelBuffer> PixelBufferCache;
PixelBufferCache pixelbuffer_cache;

PixelBuffer get_pixelbuffer(const std::string& filename)
{
  PixelBufferCache::iterator it = pixelbuffer_cache.find(filename);

  if (it == pixelbuffer_cache.end())
    return (pixelbuffer_cache[filename] = PixelBuffer::from_file(filename));
  else
    return it->second;
}

Sprite
pixelbuffer2sprite(const PixelBuffer& buffer)
{
  return Sprite(buffer);
}

Sprite
make_sprite(const std::string& filename)
{
  return Sprite(filename);
}

PixelBuffer
make_pixelbuffer(const std::string& filename)
{
  return get_pixelbuffer(filename);
}

PixelBuffer
make_region_pixelbuffer_from_resource(const std::string& filename, int x, int y, int w, int h)
{
  PixelBuffer buffer = get_pixelbuffer(filename);
  PixelBuffer target(w, h);
  clear(target);
  blit_opaque(target, buffer, -x, -y);

  return target;
}

Sprite
make_sprite_from_resource(const std::string& filename)
{
  return Sprite(filename);
}

PixelBuffer
make_pixelbuffer_from_resource(const std::string& filename)
{
  return get_pixelbuffer(filename);
}

PixelBuffer
make_pixelbuffer(int width, int height)
{
  return PixelBuffer(width, height);
}

PixelBuffer
make_region_pixelbuffer(const PixelBuffer& buffer, int x, int y, int w, int h)
{
  PixelBuffer target(w, h);
  clear(target);
  blit_opaque(target, buffer, -x, -y);

  return target;
}

PixelBuffer
scale_pixelbuffer(PixelBuffer buffer)
{
  PixelBuffer target(buffer.get_width()/2, buffer.get_height()/2);

  target.lock();
  buffer.lock();

  unsigned char* target_buf = static_cast<unsigned char*>(target.get_data());
  unsigned char* buffer_buf = static_cast<unsigned char*>(buffer.get_data());

  int width  = target.get_width();
  int height = target.get_height();
  int target_pitch = target.get_pitch();
  int buffer_pitch = buffer.get_pitch();

  for(int y = 0; y < height; ++y)
    for(int x = 0; x < width; ++x)
    {
      target_buf[target_pitch*y + 4*x + 0] = buffer_buf[buffer_pitch * y*2 + 4*x*2 + 0];
      target_buf[target_pitch*y + 4*x + 1] = buffer_buf[buffer_pitch * y*2 + 4*x*2 + 1];
      target_buf[target_pitch*y + 4*x + 2] = buffer_buf[buffer_pitch * y*2 + 4*x*2 + 2];
      target_buf[target_pitch*y + 4*x + 3] = buffer_buf[buffer_pitch * y*2 + 4*x*2 + 3];
    }

  buffer.unlock();
  target.unlock();

  return target;
}

/* EOF */
