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

/** A Tile is a surface or sprite together with information for
 *  collision detection (aka colmap). The collision map is at a
 *  resolution of 8x8 bits. Position information is handled in the
 *  TileMap and not here. (flyweight pattern). */
class Tile
{
private:
  CL_Sprite sur;
  CL_PixelBuffer pixelbuffer;

  /** Color used for the minimap to represent this tile */
  CL_Color  color;

  /** Color used on 'Show Attributes', ie. to represent walkable areas
      and such */
  CL_Color  attribute_color;

  std::string filename;
public:
  unsigned char colmap[8];

  /** @param filename Surface to use 
   *  @param arg_colmap a 8 char long array */
  Tile(const std::string& filename, 
       const CL_Color& color, 
       const CL_Color& attribute_color, 
       unsigned char* arg_colmap = NULL);
  
  ~Tile();

  CL_Sprite& get_sprite();

  /** Return a pixelbuffer associated with this tile, caller must not
      delete the pixelbuffer, the Tile will take care of that */
  CL_PixelBuffer get_pixelbuffer();

  CL_Color   get_color();
  CL_Color   get_attribute_color();

  std::string get_filename() const { return filename; }

  inline bool get_col(unsigned char x, unsigned char  y)
  {
    assert(x < 8);
    assert(y < 8);
    return (colmap[y] & (1 << (7-x)));
  }

  inline void set_col(unsigned char x, unsigned char  y, bool val)
  {
    assert(x < 8);
    assert(y < 8);
    if (val)
      colmap[y] |= (1 << (7-x));
    else
      colmap[y] &= ~(1 << (7-x));
  }

  CL_Color calc_color();
};

#endif

/* EOF */
