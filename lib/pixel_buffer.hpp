// Flexlay - A Generic 2D Game Editor
// Copyright (C) 2014 Ingo Ruhnke <grumbel@gmail.com>
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

class PixelBuffer
{
public:
  PixelBuffer()
  {}

  PixelBuffer(int width, int height)
  {}

#ifdef GRUMBEL
  CL_PixelFormat get_format() const { return m_pixelbuffer.get_format(); }

  CL_Palette get_palette() const { return m_pixelbuffer.get_palette(); }
#endif

  void lock() { }
  void unlock() { }

  int get_width() const { return 0; } ////m_pixelbuffer.get_width(); }
  int get_height() const { return  0; } ////m_pixelbuffer.get_height(); }
  int get_pitch() const { return  0; } ////m_pixelbuffer.get_pitch(); }

  void* get_data() { return  nullptr; } ////m_pixelbuffer.get_data(); }

  explicit operator bool() { return true;
#ifdef GRUMBEL
    m_pixelbuffer; 
#endif
  }
};

#endif

/* EOF */
