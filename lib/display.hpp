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

#ifndef HEADER_DISPLAY_HPP
#define HEADER_DISPLAY_HPP

class Rect;
class Color;

class Display
{
private:
public:
  static void draw_rect(const Rect& rect, const Color& color);
  static void fill_rect(const Rect& rect, const Color& color);

  static void draw_line(float x1, float y1, float x2, float y2, const Color& color);

  static void push_modelview();
  static void pop_modelview();
  static void add_translate(float x, float y);

  static void push_cliprect(const Rect& rect);
  static void pop_cliprect();

private:
  Display(const Display&) = delete;
  Display& operator=(const Display&) = delete;
};

#endif

/* EOF */
