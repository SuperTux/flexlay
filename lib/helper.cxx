//  $Id$
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

#include <ClanLib/Display/sprite_description.h>
#include <ClanLib/Display/pixel_format.h>
#include <ClanLib/Display/Providers/provider_factory.h>
#include <ClanLib/core.h>
#include "blitter.hxx"
#include "helper.hxx"

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
    desc.add_frame(CL_ProviderFactory::load(filename));
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
    return CL_ProviderFactory::load(filename);
  } catch (CL_Error& err) {
    std::cout << "CL_Error: " << err.message << std::endl;
    return CL_PixelBuffer();
  }
}

CL_PixelBuffer
make_region_pixelbuffer(const std::string& filename, int x, int y, int w, int h)
{
  try {
    CL_PixelBuffer buffer = CL_ProviderFactory::load(filename);

    CL_PixelBuffer target(w, h, w * (buffer.get_format().get_depth()/8), buffer.get_format());
    clear(target);
    blit(target, buffer, -x, -y);

    return target;
  } catch (CL_Error& err) {
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

/* EOF */
