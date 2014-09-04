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

#ifndef HEADER_FLEXLAY_HELPER_HPP
#define HEADER_FLEXLAY_HELPER_HPP

#include "sprite.hpp"
#include "pixel_buffer.hpp"

Sprite pixelbuffer2sprite(const PixelBuffer& buffer);
Sprite make_sprite(const std::string& filename);
PixelBuffer make_pixelbuffer(const std::string& filename);
PixelBuffer make_region_pixelbuffer_from_resource(const std::string& filename, int x, int y, int w, int h);
PixelBuffer make_region_pixelbuffer(const PixelBuffer& buffer, int x, int y, int w, int h);
PixelBuffer make_pixelbuffer(int width, int height);
PixelBuffer scale_pixelbuffer(PixelBuffer buffer);

Sprite make_sprite_from_resource(const std::string& filename, CL_ResourceManager& resources);
PixelBuffer make_pixelbuffer_from_resource(const std::string& filename, CL_ResourceManager& resources);
#endif

/* EOF */
