// Flexlay - A Generic 2D Game Editor
// Copyright (C) 2014 Ingo Ruhnke <grumbel@gmx.de>
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

#ifndef HEADER_PIXEL_BUFFER_HPP
#define HEADER_PIXEL_BUFFER_HPP

#include <ClanLib/Display/palette.h>
#include <ClanLib/Display/pixel_buffer.h>
#include <ClanLib/Display/pixel_format.h>

class PixelBuffer
{
private:
  CL_PixelBuffer m_pixelbuffer;

public:
  PixelBuffer() :
    m_pixelbuffer()
  {}

  PixelBuffer(CL_PixelBuffer pixelbuffer) :
    m_pixelbuffer(pixelbuffer)
  {}

  PixelBuffer(int width, int height) :
    m_pixelbuffer(width, height, width*4, CL_PixelFormat::rgba8888)
  {}
  
  CL_PixelFormat get_format() const { return m_pixelbuffer.get_format(); }

  CL_Palette get_palette() const { return m_pixelbuffer.get_palette(); }
  
  int get_width() const { return m_pixelbuffer.get_width(); }
  int get_height() const { return m_pixelbuffer.get_height(); }
  int get_pitch() const { return m_pixelbuffer.get_pitch(); }

  void* get_data() { return m_pixelbuffer.get_data(); }

  void lock() { m_pixelbuffer.lock(); }
  void unlock() { m_pixelbuffer.unlock(); }

  explicit operator bool() { return m_pixelbuffer; }

  CL_PixelBuffer to_cl() const { return m_pixelbuffer; }
};

#endif

/* EOF */
