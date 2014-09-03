//  Flexlay - A Generic 2D Game Editor
//  Copyright (C) 2002 Ingo Ruhnke <grumbel@gmx.de>
//
//  This program is free software: you can redistribute it and/or modify
//  it under the terms of the GNU General Public License as published by
//  the Free Software Foundation, either version 3 of the License, or
//  (at your option) any later version.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//  GNU General Public License for more details.
//
//  You should have received a copy of the GNU General Public License
//  along with this program.  If not, see <http://www.gnu.org/licenses/>.

#include "box.hpp"

#include <ClanLib/Display/display.h>

#include "color.hpp"
#include "math/rect.hpp"

Color background   (210, 210, 210);
Color background_hl(240, 240, 240);
Color background_sw(200, 200, 200);
Color highlight    (255, 255, 255);
Color midtone      (150, 150, 150);
Color shadow       (100, 100, 100);

void
Box::draw_button_up(const Rect& rect)
{
  CL_Display::fill_rect(rect.to_cl(), background_hl.to_cl());
  CL_Display::draw_line(rect.left, rect.top,
                        rect.right, rect.top, highlight.to_cl());
  CL_Display::draw_line(rect.left, rect.top,
                        rect.left, rect.bottom, highlight.to_cl());

  CL_Display::draw_line(rect.left, rect.bottom,
                        rect.right, rect.bottom, shadow.to_cl());
  CL_Display::draw_line(rect.right, rect.top,
                        rect.right, rect.bottom, shadow.to_cl());
}

void
Box::draw_button_down(const Rect& rect)
{
  CL_Display::fill_rect(rect.to_cl(), background_sw.to_cl());

  CL_Display::draw_line(rect.left, rect.bottom,
                          rect.right, rect.bottom, highlight.to_cl());
  CL_Display::draw_line(rect.right, rect.top,
                        rect.right, rect.bottom, highlight.to_cl());

  CL_Display::draw_line(rect.left, rect.top,
                        rect.right, rect.top, shadow.to_cl());
  CL_Display::draw_line(rect.left, rect.top,
                        rect.left, rect.bottom, shadow.to_cl());
}

void
Box::draw_button_neutral(const Rect& rect)
{
  CL_Display::fill_rect(rect.to_cl(), background.to_cl());
}

void
Box::draw_panel(const Rect& rect)
{
  CL_Display::fill_rect(rect.to_cl(), background.to_cl());
  CL_Display::draw_line(rect.left, rect.top,
                        rect.right, rect.top, highlight.to_cl());
  CL_Display::draw_line(rect.left, rect.top,
                        rect.left, rect.bottom, highlight.to_cl());

  CL_Display::draw_line(rect.left, rect.bottom,
                        rect.right, rect.bottom, shadow.to_cl());
  CL_Display::draw_line(rect.right, rect.top,
                        rect.right, rect.bottom, shadow.to_cl());
}

void
Box::draw_panel_down(const Rect& rect)
{
  CL_Display::fill_rect(rect.to_cl(), background.to_cl());
  CL_Display::draw_line(rect.left, rect.top,
                        rect.right, rect.top, shadow.to_cl());
  CL_Display::draw_line(rect.left, rect.top,
                        rect.left, rect.bottom, shadow.to_cl());

  CL_Display::draw_line(rect.left, rect.bottom,
                        rect.right, rect.bottom, highlight.to_cl());
  CL_Display::draw_line(rect.right, rect.top,
                        rect.right, rect.bottom, highlight.to_cl());
}

void
Box::draw_window(const Rect& rect)
{
  draw_panel(Rect(rect.left+1, rect.top+1, rect.right-2, rect.bottom-2));
  CL_Display::draw_rect(rect.to_cl(), Color(0, 0, 0).to_cl());
}

/* EOF */
