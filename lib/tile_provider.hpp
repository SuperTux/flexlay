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

#ifndef HEADER_FLEXLAY_TILE_PROVIDER_HPP
#define HEADER_FLEXLAY_TILE_PROVIDER_HPP

#include <memory>

#include "sprite.hpp"
#include "pixel_buffer.hpp"

class TileProviderImpl;

/** TileProvider provides a flexible way to perform load-on-demand for Tiles */
class TileProvider
{
public:
  TileProvider() {}
  TileProvider(TileProviderImpl* impl);

  Sprite      get_sprite() const;
  PixelBuffer get_pixelbuffer() const;

  explicit operator bool() const { return impl.get(); }

private:
  std::shared_ptr<TileProviderImpl> impl;
};

#endif

/* EOF */
