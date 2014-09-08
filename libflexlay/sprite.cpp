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

#include "sprite.hpp"

#include <QPainter>

#include <iostream>
#include <assert.h>

#include "graphic_context.hpp"
#include "pixel_buffer.hpp"

class SpriteImpl
{
public:
  PixelBuffer buffer;
  std::string filename;

  Origin origin;
  Point pos;

  SpriteImpl() :
    origin(Flexlay_origin_top_left),
    pos()
  {}
};

Sprite::Sprite() :
  m_impl()
{}

Sprite::Sprite(const std::string& filename) :
  m_impl(new SpriteImpl)
{
  m_impl->buffer = PixelBuffer::from_file(filename);
  m_impl->filename = filename;
}

Sprite::Sprite(const PixelBuffer& buffer) :
  m_impl(new SpriteImpl)
{
  m_impl->buffer = buffer;
  m_impl->filename = "<PixelBuffer>";
}

void
Sprite::draw(float x, float y, GraphicContext& gc)
{
  assert(m_impl);
  //std::cout << "Sprite::draw: " << x << " " << y << " isNull:" << m_impl->buffer.get_qimage().isNull() << std::endl;
  QPainter& painter = gc.get_qt_painter();
  QImage img = m_impl->buffer.get_qimage();
  if (img.isNull())
  {
    std::cout << "Error: Sprite: Empty PixelBuffer: " << m_impl->filename << std::endl;
  }
  else
  {
    Point origin = calc_origin(m_impl->origin, Size(get_width(), get_height()));

    painter.drawImage(QPoint(x - origin.x, y - origin.y), img);
  }
}

int
Sprite::get_width() const
{
  assert(m_impl);
  return m_impl->buffer.get_width();
}

int
Sprite::get_height() const
{
  assert(m_impl);
  return m_impl->buffer.get_height();
}

void
Sprite::set_scale(float x, float y)
{
  //m_sprite.set_scale(x, y);
}

void
Sprite::set_blend_func(BlendFunc src, BlendFunc dest)
{
  //m_sprite.set_blend_func(src, dest);
}

void
Sprite::set_blend_func_separate(BlendFunc src, BlendFunc dest,
                                BlendFunc src_alpha, BlendFunc dest_alpha)
{
  //m_sprite.set_blend_func_separate(src, dest, src_alpha, dest_alpha);
}

void
Sprite::set_color(const Color& color)
{
  //m_sprite.set_color(color.to_cl());
}

void
Sprite::set_alpha(float alpha)
{
  //m_sprite.set_alpha(alpha);
}

void
Sprite::set_alignment(Origin origin, int x, int y)
{
  m_impl->origin = origin;
  m_impl->pos = Point(x, y);
}

void
Sprite::set_angle(float angle)
{
  //m_sprite.set_angle(angle);
}

void
Sprite::get_alignment(Origin& origin, int& x, int& y) const
{
  origin = m_impl->origin = origin;
  x = m_impl->pos.x;
  y = m_impl->pos.y;
}

void
Sprite::get_scale(float& x, float& y) const
{
  //m_sprite.get_scale(x, y);
}

void
Sprite::add_frame(Surface surface, const Rect& rect)
{
  //m_sprite.add_frame(surface.to_cl(), rect.to_cl());
}

PixelBuffer
Sprite::get_pixelbuffer() const
{
  return m_impl->buffer;
}

Sprite::operator bool() const
{
  return static_cast<bool>(m_impl);
}
/* EOF */
