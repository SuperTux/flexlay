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

#include "pixel_buffer.hpp"

#include <QImage>
#include <iostream>

class PixelBufferImpl
{
public:
  QImage image;
};

PixelBuffer
PixelBuffer::from_file(const std::string& filename_)
{
  // FIXME: hack
  std::string filename = "../data/images/icons16/" + filename_ + ".png";

  PixelBuffer buffer;
  buffer.m_impl.reset(new PixelBufferImpl);
  buffer.m_impl->image = QImage(filename.c_str());

  std::cout << "loading: " << filename << " -> " << buffer.m_impl->image.isNull() << std::endl;

  return buffer;
}

PixelBuffer::PixelBuffer()
{}

PixelBuffer::PixelBuffer(int width, int height) :
  m_impl(new PixelBufferImpl)
{
  m_impl->image = QImage(QSize(width, height),QImage::Format_ARGB32);
}

#ifdef GRUMBEL
  CL_PixelFormat get_format() const { return m_pixelbuffer.get_format(); }

  CL_Palette get_palette() const { return m_pixelbuffer.get_palette(); }
#endif

QImage&
PixelBuffer::get_qimage()
{
  return m_impl->image;  
}

void
PixelBuffer::lock()
{ 
}

void
PixelBuffer::unlock()
{ 
}

int
PixelBuffer::get_width() const
{
  return m_impl->image.width(); 
}

int
PixelBuffer::get_height() const
{ 
  return m_impl->image.height(); 

}
int
PixelBuffer::get_pitch() const
{
  return m_impl->image.bytesPerLine();
}

void*
PixelBuffer::get_data()
{
  return m_impl->image.bits();
}

/* EOF */
