//  $Id: tile.cxx,v 1.4 2003/09/22 18:37:05 grumbel Exp $
//
//  Windstille - A Jump'n Shoot Game
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

#include <ClanLib/Core/System/error.h>
#include <ClanLib/Display/sprite_description.h>
#include <ClanLib/Display/pixel_buffer.h>
#include <ClanLib/Display/pixel_format.h>
#include <ClanLib/Display/palette.h>
#include <ClanLib/Display/Providers/provider_factory.h>
#include <iostream>
#include "globals.hxx"
#include "../string_converter.hxx"
#include "tile.hxx"

Tile::Tile(const std::string& filename_, 
           const CL_Color& color_, 
           const CL_Color& attribute_color_, 
           unsigned char arg_colmap[])
  : pixelbuffer(0),
    color(color_),
    attribute_color(attribute_color_),
    filename(filename_)
{
  // FIXME: Kind of evil singular value
  if (color == CL_Color(254, 254, 254, 254))
    {
      color = calc_color();
    }
  
  //sur.set_alignment(origin_center, 0, 0);
  memcpy(colmap, arg_colmap, 8);
}

Tile::~Tile()
{
  delete pixelbuffer;
}

CL_Color
Tile::get_color()
{
  return color;
}

CL_Color
Tile::get_attribute_color()
{
  return attribute_color;
}

CL_Sprite&
Tile::get_sprite()
{
  if (sur)
    return sur;
  else
    {
      try {
        //std::cout << "Loading Tile: " << filename << std::endl;
        if (has_suffix(filename, ".png") || has_suffix(filename, ".jpg"))
          {
            CL_SpriteDescription desc;
            desc.add_frame(get_pixelbuffer(), false);
            sur = CL_Sprite(desc);
          }
        else
          {
            sur = CL_Sprite(filename, resources);
          }
        return sur;
      } catch (CL_Error& err) {
        std::cout << "Tile: CL_Error: " << err.message << std::endl;
        assert(0);
      }
    }
}

CL_PixelBuffer*
Tile::get_pixelbuffer()
{	
  if (pixelbuffer)
    return pixelbuffer;
  {
    if (has_suffix(filename, ".png") || has_suffix(filename, ".jpg"))
      {
        pixelbuffer = CL_ProviderFactory::load(filename);
      }
    else
      {
        CL_SpriteDescription descr(filename, resources);
        pixelbuffer = new CL_PixelBuffer(*(descr.get_frames().begin()->first));
      }
    return pixelbuffer;
  }
}

CL_Color
Tile::calc_color()
{
  CL_PixelBuffer* buffer = get_pixelbuffer();
  buffer->lock();
  unsigned char* buf = static_cast<unsigned char*>(buffer->get_data());
  int len = buffer->get_height() * buffer->get_width();

  int red   = 0;
  int green = 0;
  int blue  = 0;
  int alpha = 0;
  
  switch (buffer->get_format().get_depth())
    {
    case 8:
      {
        CL_Palette palette = buffer->get_palette();
        for(int i = 0; i < len; ++i)
          {
            red   += palette.colors[buf[i]].get_red();
            green += palette.colors[buf[i]].get_green();
            blue  += palette.colors[buf[i]].get_blue();
            alpha += 255;
          }
      }
      break;
    case 24:
      for(int i = 0; i < len; ++i)
        {
          red   += buf[3*i + 0];
          green += buf[3*i + 1];
          blue  += buf[3*i + 2];
          alpha += 255;
        }
      break;
    case 32:
      for(int i = 0; i < len; ++i)
        {
          int a = buf[4*i + 0];
          alpha += a;
          red   += buf[4*i + 3]*a/255;;
          green += buf[4*i + 2]*a/255;;
          blue  += buf[4*i + 1]*a/255;;
        }
      break;
    }

  buffer->unlock();

  return CL_Color(static_cast<int>(red   / len),
                  static_cast<int>(green / len),
                  static_cast<int>(blue  / len),
                  static_cast<int>(alpha / len));
}

/* EOF */
