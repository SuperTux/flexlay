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

#ifndef HEADER_GRAPHIC_CONTEXT_HPP
#define HEADER_GRAPHIC_CONTEXT_HPP

class GraphicContextState;
class CL_GraphicContext;
class Color;
class Rectf;

class GraphicContext
{
private:
public:
  GraphicContext(GraphicContextState& state, CL_GraphicContext* gc);

  void clear(const Color& color);
  void draw_rect(const Rectf& rect, const Color& color);
  void fill_rect(const Rectf& rect, const Color& color);

  void draw_line(float x1, float y1, float x2, float y2, const Color& color);

  void push_modelview();
  void pop_modelview();
  void add_translate(float x, float y);

  void flush();

public:
  GraphicContextState& state;
  CL_GraphicContext* gc;

private:
  GraphicContext(const GraphicContext&) = delete;
  GraphicContext& operator=(const GraphicContext&) = delete;
};

#endif

/* EOF */
