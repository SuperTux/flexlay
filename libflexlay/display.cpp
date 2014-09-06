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

#include "display.hpp"

#include "color.hpp"
#include "math/rect.hpp"

void
Display::clear(const Color& color)
{
  ////CL_Display::clear(color.to_cl());
}

void
Display::draw_rect(const Rectf& rect, const Color& color)
{
  ////CL_Display::draw_rect(rect.to_cl(), color.to_cl());
}

void
Display::fill_rect(const Rectf& rect, const Color& color)
{
  ////CL_Display::fill_rect(rect.to_cl(), color.to_cl());
}

void
Display::draw_line(float x1, float y1, float x2, float y2, const Color& color)
{
  ////CL_Display::draw_line(x1, y1, x2, y2, color.to_cl());
}

void
Display::push_modelview()
{
  ////CL_Display::push_modelview();
}

void
Display::pop_modelview()
{
  ////CL_Display::pop_modelview();
}

void
Display::add_translate(float x, float y)
{
  ////CL_Display::add_translate(x, y);
}

void
Display::set_cliprect(const Rect& rect)
{
  ////CL_Display::set_cliprect(rect.to_cl());
}

void
Display::push_cliprect(const Rect& rect)
{
  ////CL_Display::push_cliprect(rect.to_cl());
}

void
Display::pop_cliprect()
{
  ////CL_Display::pop_cliprect();
}

int
Display::get_width()
{
  return 0; ////return CL_Display::get_width();
}

int
Display::get_height()
{
  return 0; ////return CL_Display::get_height();
}

void
Display::flush()
{
  ////CL_Display::flush();
}

/* EOF */
