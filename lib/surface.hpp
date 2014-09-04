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

#include <ClanLib/Display/surface.h>

#include "math/rect.hpp"
#include "pixel_buffer.hpp"

class Surface
{
private:
  CL_Surface m_surface;

public:
  Surface() :
    m_surface()
  {}

  Surface(PixelBuffer pixelbuffer) :
    m_surface(pixelbuffer.to_cl())
  {}

  void draw(const Rect& rect, CL_GraphicContext* gc = 0)
  {
    m_surface.draw(rect.to_cl(), gc);
  }

  void draw(float x, float y, CL_GraphicContext* gc = 0)
  {
    m_surface.draw(x, y, gc);
  }

  void set_alignment(CL_Origin origin, int x = 0, int y = 0)
  {
    m_surface.set_alignment(origin, x, y);
  }

  void set_alpha(float alpha)
  {
    m_surface.set_alpha(alpha);
  }

  void set_scale(float x, float y)
  {
    m_surface.set_scale(x, y);
  }

  void set_blend_func(CL_BlendFunc src, CL_BlendFunc dest)
  {
    m_surface.set_blend_func(src, dest);
  }

  int get_width() const { return m_surface.get_width(); }
  int get_height() const { return m_surface.get_height(); }

  CL_Surface to_cl() { return m_surface; }

  explicit operator bool() const
  {
    return m_surface;
  }
};

#endif

/* EOF */
