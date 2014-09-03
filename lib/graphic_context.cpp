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

#include "graphic_context.hpp"

#include <ClanLib/Display/graphic_context.h>

#include "color.hpp"
#include "math/rect.hpp"

GraphicContext::GraphicContext(GraphicContextState& state_, CL_GraphicContext* gc_) :
  state(state_),
  gc(gc_)
{
}

void
GraphicContext::clear(const Color& color)
{
  gc->clear(color.to_cl());
}

void
GraphicContext::draw_rect(const Rectf& rect, const Color& color)
{
  gc->draw_rect(rect.to_cl(), color.to_cl());
}

void
GraphicContext::fill_rect(const Rectf& rect, const Color& color)
{
  gc->fill_rect(rect.to_cl(), color.to_cl());
}

void
GraphicContext::draw_line(float x1, float y1, float x2, float y2, const Color& color)
{ 
  gc->draw_line(x1, y1, x2, y2, color.to_cl());
}

void
GraphicContext::push_modelview()
{
  gc->push_modelview();
}

void
GraphicContext::pop_modelview()
{
  gc->pop_modelview();
}

void
GraphicContext::add_translate(float x, float y)
{
  gc->add_translate(x, y);
}

void
GraphicContext::flush()
{
  gc->flush();
}

/* EOF */
