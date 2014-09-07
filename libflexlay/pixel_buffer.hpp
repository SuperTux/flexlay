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

#include <memory>
#include <string>

class PixelBufferImpl;
class QImage;

class PixelBuffer
{
public:
  static PixelBuffer from_file(const std::string& filename);

public:
  PixelBuffer();
  PixelBuffer(int width, int height);

#ifdef GRUMBEL
  CL_PixelFormat get_format() const { return m_pixelbuffer.get_format(); }

  CL_Palette get_palette() const { return m_pixelbuffer.get_palette(); }
#endif

void lock();
void unlock();

  int get_width() const;
  int get_height() const;
  int get_pitch() const;

  int get_depth() const;

  void* get_data();

  explicit operator bool() { return static_cast<bool>(m_impl); }

  QImage& get_qimage();

private:
  std::shared_ptr<PixelBufferImpl> m_impl;
};

#endif

/* EOF */
