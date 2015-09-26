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

#ifndef HEADER_FLEXLAY_TILE_HPP
#define HEADER_FLEXLAY_TILE_HPP

#include <memory>

#include "color.hpp"

class TileImpl;
class TileProvider;
class PixelBuffer;
class Sprite;

/** A Tile is a surface or sprite together with meta information for
    collision (aka colmap), walkability or such. */
class Tile
{
public:
  Tile(const TileProvider& provider);

  Tile(const PixelBuffer& pixelbuffer);

  Tile(const PixelBuffer& pixelbuffer,
       const Sprite& sprite);

  /** @param filename Surface to use
   *  @param arg_colmap a 8 char long array */
  Tile(const std::string& filename,
       const Color& attribute_color);
  ~Tile();

  Sprite& get_sprite();

  /** Return a pixelbuffer associated with this tile, caller must not
      delete the pixelbuffer, the Tile will take care of that */
  PixelBuffer get_pixelbuffer();

  // FIXME: Document all those functions
  Color get_color();
  Color get_attribute_color();

  std::string get_filename() const;

  bool get_col(unsigned char x, unsigned char  y);
  void set_col(unsigned char x, unsigned char  y, bool val);

  Color calc_color();

private:
  std::shared_ptr<TileImpl> impl;
};

#endif

/* EOF */
