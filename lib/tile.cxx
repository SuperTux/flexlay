//  $Id: tile.cxx,v 1.4 2003/09/22 18:37:05 grumbel Exp $
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

#include <ClanLib/Core/System/error.h>
#include <ClanLib/Display/sprite_description.h>
#include <ClanLib/Display/pixel_buffer.h>
#include <ClanLib/Display/pixel_format.h>
#include <ClanLib/Display/palette.h>
#include <ClanLib/Display/Providers/provider_factory.h>
#include <iostream>
#include "string_converter.hxx"
#include "tile_provider.hxx"
#include "tile.hxx"

class TileImpl
{
public:
  TileProvider   provider;

  CL_Sprite      sprite;
  CL_PixelBuffer pixelbuffer;

  bool transparent;
  bool has_color;

  /** Color used for the minimap to represent this tile */
  CL_Color  color;

  /** Color used on 'Show Attributes', ie. to represent walkable areas
      and such */
  CL_Color  attribute_color;

  // FIXME: old windstille stuff
  unsigned char colmap[8];

  std::string filename;
};

Tile::Tile(const TileProvider& provider)
  : impl(new TileImpl())
{
  impl->provider  = provider;
  impl->has_color = false; 
}

Tile::Tile(const CL_PixelBuffer& pixelbuffer,
           const CL_Sprite& sprite)
  : impl(new TileImpl())
{
  impl->pixelbuffer = pixelbuffer;
  impl->sprite      = sprite;
  impl->has_color   = false;
}

Tile::Tile(const CL_PixelBuffer& pixelbuffer)
  : impl(new TileImpl())
{
  impl->pixelbuffer = pixelbuffer;
  impl->has_color   = false;
}

Tile::Tile(const std::string& filename_, 
           const CL_Color& attribute_color_)
  : impl(new TileImpl())
{
  impl->has_color = false;
  impl->attribute_color = attribute_color_;
  impl->filename = filename_;
}

Tile::~Tile()
{
}

CL_Color
Tile::get_color()
{
  if (impl->has_color)
    {
      return impl->color;
    }
  else
    {
      impl->color = calc_color();
      impl->has_color = true;
      return impl->color;
    }
}

CL_Color
Tile::get_attribute_color()
{
  return impl->attribute_color;
}

CL_Sprite&
Tile::get_sprite()
{
  if (impl->sprite)
    {
      return impl->sprite;
    }
  else
    {
      if (impl->provider)
        {
          impl->sprite = impl->provider.get_sprite();
        }
      else
        {
          CL_SpriteDescription desc;
          desc.add_frame(CL_PixelBuffer(get_pixelbuffer()));
          impl->sprite = CL_Sprite(desc);
        }

      return impl->sprite;
    }
}

CL_PixelBuffer
Tile::get_pixelbuffer()
{
  if (impl->pixelbuffer)
    {
      return impl->pixelbuffer;
    }
  else 
    {
      if (impl->provider)
        {
          impl->pixelbuffer = impl->provider.get_pixelbuffer();
          return impl->pixelbuffer;
        }
      else
        {
          // FIXME: Move all this into a special provider

          try {
            if (has_suffix(impl->filename, ".png") || has_suffix(impl->filename, ".jpg"))
              {
                impl->pixelbuffer = CL_PixelBuffer(CL_ProviderFactory::load(impl->filename));
              }
            else
              {
                //CL_SpriteDescription descr(impl->filename, resources);
                //impl->pixelbuffer = CL_PixelBuffer(*(descr.get_frames().begin()->first));
                assert(0);
              }
            return impl->pixelbuffer;
          
          } catch(CL_Error& err) {
            std::cout << "CL_Error: " << err.message << std::endl;
            std::cout << "          filename = " << impl->filename << std::endl;
            return CL_PixelBuffer();
          }
        }
    }
}

CL_Color
Tile::calc_color()
{
  CL_PixelBuffer buffer = get_pixelbuffer();
  buffer.lock();
  unsigned char* buf = static_cast<unsigned char*>(buffer.get_data());
  int len = buffer.get_height() * buffer.get_width();

  int red   = 0;
  int green = 0;
  int blue  = 0;
  int alpha = 0;
  
  switch (buffer.get_format().get_depth())
    {
    case 8:
      {
        CL_Palette palette = buffer.get_palette();
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

  buffer.unlock();

  return CL_Color(static_cast<int>(red   / len),
                  static_cast<int>(green / len),
                  static_cast<int>(blue  / len),
                  static_cast<int>(alpha / len));
}

bool
Tile::get_col(unsigned char x, unsigned char  y)
{
  assert(x < 8);
  assert(y < 8);
  return (impl->colmap[y] & (1 << (7-x)));
}

void
Tile::set_col(unsigned char x, unsigned char  y, bool val)
{
  assert(x < 8);
  assert(y < 8);
  if (val)
    impl->colmap[y] |= (1 << (7-x));
  else
    impl->colmap[y] &= ~(1 << (7-x));
}

std::string
Tile::get_filename() const
{
  return impl->filename; 
}

/* EOF */
