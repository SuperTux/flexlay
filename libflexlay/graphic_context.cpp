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

#include "graphic_context.hpp"

#include <QPainter>

#include "color.hpp"
#include "graphic_context_state.hpp"
#include "math/rect.hpp"

GraphicContext::GraphicContext(QPainter& painter) :
  m_state(nullptr),
  m_painter(painter)
{
}

GraphicContext::GraphicContext(const GraphicContextState& state, QPainter& painter) :
  m_state(&state),
  m_painter(painter)
{
}

void
GraphicContext::clear(const Color& color)
{
#ifdef GRUMBEL
  gc->clear(color.to_cl());
#endif
}

void
GraphicContext::draw_rect(const Rectf& rect, const Color& color)
{
  m_painter.setPen(color.to_qt());
  m_painter.drawRect(rect.to_qt());
}

void
GraphicContext::fill_rect(const Rectf& rect, const Color& color)
{
  m_painter.fillRect(rect.to_qt(), color.to_qt());
}

void
GraphicContext::draw_line(float x1, float y1, float x2, float y2, const Color& color)
{
  m_painter.setPen(color.to_qt());
  m_painter.drawLine(QLineF(x1, y1, x2, y2));
}

void
GraphicContext::push_modelview()
{
  m_painter.save();
}

void
GraphicContext::pop_modelview()
{
  m_painter.restore();
}

void
GraphicContext::translate(float x, float y)
{
  m_painter.setViewTransformEnabled(true);
  m_painter.translate(x, y);
}

void
GraphicContext::scale(float x, float y)
{
  m_painter.setViewTransformEnabled(true);
  m_painter.scale(x, y);
}

void
GraphicContext::rotate(float angle)
{
  m_painter.setViewTransformEnabled(true);
  m_painter.rotate(angle);
}

Rectf
GraphicContext::get_clip_rect() const
{
  if (m_state)
    return m_state->get_clip_rect();
  else
    return Rectf();
}

void
GraphicContext::flush()
{
}

/* EOF */
