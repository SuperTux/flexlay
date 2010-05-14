//  Flexlay - A Generic 2D Game Editor
//  Copyright (C) 2002 Ingo Ruhnke <grumbel@gmx.de>
//
//  This program is free software: you can redistribute it and/or modify
//  it under the terms of the GNU General Public License as published by
//  the Free Software Foundation, either version 3 of the License, or
//  (at your option) any later version.
//  
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//  GNU General Public License for more details.
//  
//  You should have received a copy of the GNU General Public License
//  along with this program.  If not, see <http://www.gnu.org/licenses/>.

#include <ClanLib/Display/sprite_description.h>
#include <ClanLib/Display/pixel_format.h>
#include <ClanLib/Display/Providers/provider_factory.h>
#include <ClanLib/core.h>

#include "blitter.hpp"
#include "globals.hpp"
#include "helper.hpp"

typedef std::map<std::string, CL_PixelBuffer> PixelBufferCache;
PixelBufferCache pixelbuffer_cache;

CL_PixelBuffer get_pixelbuffer(const std::string& filename)
{
  PixelBufferCache::iterator it = pixelbuffer_cache.find(filename);
  
  if (it == pixelbuffer_cache.end())
    return (pixelbuffer_cache[filename] = CL_ProviderFactory::load(filename));
  else
    return it->second;
}

CL_Sprite
pixelbuffer2sprite(const CL_PixelBuffer& buffer)
{
  CL_SpriteDescription desc;
  desc.add_frame(buffer);
  return CL_Sprite(desc);
}

CL_Sprite
make_sprite(const std::string& filename)
{
  try {
    CL_SpriteDescription desc;
    desc.add_frame(get_pixelbuffer(filename));
    return CL_Sprite(desc);
  } catch (CL_Error& err) {
    std::cout << "CL_Error: " << err.message << std::endl;
    return CL_Sprite();
  }
}

CL_PixelBuffer
make_pixelbuffer(const std::string& filename)
{
  try {
    return get_pixelbuffer(filename);
  } catch (CL_Error& err) {
    std::cout << "CL_Error: " << err.message << std::endl;
    return CL_PixelBuffer();
  }
}

CL_PixelBuffer
make_region_pixelbuffer_from_resource(const std::string& filename, int x, int y, int w, int h)
{
  try 
  {
    CL_PixelBuffer buffer = get_pixelbuffer(filename);
    CL_PixelBuffer target(w, h, w * 4, CL_PixelFormat::rgba8888);
    clear(target);
    blit_opaque(target, buffer, -x, -y);

    return target;
  } 
  catch (CL_Error& err) 
  {
    std::cout << "CL_Error: " << err.message << std::endl;
    return CL_PixelBuffer();
  }
}

CL_Sprite
make_sprite_from_resource(const std::string& filename, CL_ResourceManager& resources)
{
  try {
    return CL_Sprite(filename, &resources);
  } catch (CL_Error& err) {
    std::cout << "CL_Error: " << err.message << std::endl;
    return CL_Sprite();
  }  
}

CL_PixelBuffer
make_pixelbuffer_from_resource(const std::string& filename, CL_ResourceManager& resources)
{
  try {
    // FIXME: expects a sprite, won't work with 'surface'
    CL_SpriteDescription descr(filename, &resources);
    return CL_PixelBuffer(descr.get_frames().begin()->first);
  } catch (CL_Error& err) {
    std::cout << "CL_Error: " << err.message << std::endl;
    return CL_PixelBuffer();
  }
}

CL_PixelBuffer
make_pixelbuffer(int width, int height)
{
  return CL_PixelBuffer(width, height, width*4, CL_PixelFormat::rgba8888);
}

CL_PixelBuffer
make_region_pixelbuffer(const CL_PixelBuffer& buffer, int x, int y, int w, int h)
{
  try {
    CL_PixelBuffer target(w, h, w * 4, CL_PixelFormat::rgba8888);
    clear(target);
    blit_opaque(target, buffer, -x, -y);

    return target;
  } catch (CL_Error& err) {
    std::cout << "CL_Error: " << err.message << std::endl;
    return CL_PixelBuffer();
  }
}

CL_PixelBuffer
scale_pixelbuffer(CL_PixelBuffer buffer)
{
  CL_PixelBuffer target(buffer.get_width()/2, buffer.get_height()/2, (buffer.get_width()/2)*4, 
                        CL_PixelFormat::rgba8888);

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
