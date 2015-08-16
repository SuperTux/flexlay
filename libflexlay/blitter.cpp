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

#include "blitter.hpp"

#include <QPainter>
#include <QImage>

#include "pixel_buffer.hpp"

void
blit_opaque(PixelBuffer target, PixelBuffer brush, int x, int y)
{
  QPainter painter(&target.get_qimage());
  painter.setCompositionMode(QPainter::CompositionMode_Source);
  painter.drawImage(QPoint(x, y), brush.get_qimage());
}

void
blit(PixelBuffer target, PixelBuffer brush, int x, int y)
{
  QPainter painter(&target.get_qimage());
  painter.setCompositionMode(QPainter::CompositionMode_SourceOver);
  painter.drawImage(QPoint(x, y), brush.get_qimage());
}

void clear(PixelBuffer canvas)
{
  unsigned char* buffer;

  canvas.lock();
  buffer = static_cast<unsigned char*>(canvas.get_data());
  memset(buffer, 0, sizeof(unsigned char) * canvas.get_pitch() * canvas.get_height());
  canvas.unlock();
}

/* EOF */