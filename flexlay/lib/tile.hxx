//  $Id: tile.hxx,v 1.6 2003/09/22 18:37:05 grumbel Exp $
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

#ifndef HEADER_TILE_HXX
#define HEADER_TILE_HXX

#include <assert.h>
#include <ClanLib/Display/sprite.h>
#include <ClanLib/Display/pixel_buffer.h>
#include "shared_ptr.hxx"

class TileImpl;
class TileProvider;

/** A Tile is a surface or sprite together with meta information for
    collision (aka colmap), walkability or such. */
class Tile
{
public:
  Tile(const TileProvider& provider);
  
  Tile(const CL_PixelBuffer& pixelbuffer);

  Tile(const CL_PixelBuffer& pixelbuffer,
       const CL_Sprite& sprite);

  /** @param filename Surface to use 
   *  @param arg_colmap a 8 char long array */
  Tile(const std::string& filename, 
       const CL_Color& attribute_color);
  ~Tile();

  CL_Sprite& get_sprite();

  /** Return a pixelbuffer associated with this tile, caller must not
      delete the pixelbuffer, the Tile will take care of that */
  CL_PixelBuffer get_pixelbuffer();

  // FIXME: Document all those functions
  CL_Color   get_color();
  CL_Color   get_attribute_color();

  std::string get_filename() const;

  bool get_col(unsigned char x, unsigned char  y);
  void set_col(unsigned char x, unsigned char  y, bool val);

  CL_Color calc_color();

private:
  SharedPtr<TileImpl> impl;
};

#endif

/* EOF */
