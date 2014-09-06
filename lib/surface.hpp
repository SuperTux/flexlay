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

#ifndef HEADER_SURFACE_HPP
#define HEADER_SURFACE_HPP

#include "math/rect.hpp"
#include "math/origin.hpp"
#include "pixel_buffer.hpp"

enum class BlendFunc
{
one, 
one_minus_src_alpha
};

class Surface
{
public:
  Surface()
  {}

  Surface(PixelBuffer pixelbuffer)
  {}

  void draw(const Rect& rect)
  {
  }

  void draw(float x, float y)
  {
  }

  void set_alignment(Origin origin, int x = 0, int y = 0)
  {
  }

  void set_alpha(float alpha)
  {
  }

  void set_scale(float x, float y)
  {
  }

  void set_blend_func(BlendFunc src, BlendFunc dest)
  {
  }

  int get_width() const { return 0; }
  int get_height() const { return 0; }

  explicit operator bool() const
  {
    return true;
  }
};

#endif

/* EOF */
