//  $Id$
//
//  Flexlay - A Generic 2D Game Editor
//  Copyright (C) 2002 Ingo Ruhnke <grumbel@gmx.de>
//
//  This program is free software; you can redistribute it and/or
//  modify it under the terms of the GNU General Public License
//  as published by the Free Software Foundation; either version 2
//  of the License, or (at your option) any later version.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//  GNU General Public License for more details.
//
//  You should have received a copy of the GNU General Public License
//  along with this program; if not, write to the Free Software
//  Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.

#include <ClanLib/Display/color.h>
#include <ClanLib/Display/display.h>
#include "box.hxx"

CL_Color background   (210, 210, 210);
CL_Color background_hl(240, 240, 240);
CL_Color background_sw(200, 200, 200);
CL_Color highlight    (255, 255, 255);
CL_Color midtone      (150, 150, 150);
CL_Color shadow       (100, 100, 100);

void
Box::draw_button_up(const CL_Rect& rect)
{
  CL_Display::fill_rect(rect, background_hl);
  CL_Display::draw_line(rect.left, rect.top, 
                        rect.right, rect.top, highlight);
  CL_Display::draw_line(rect.left, rect.top, 
                        rect.left, rect.bottom, highlight);

  CL_Display::draw_line(rect.left, rect.bottom, 
                        rect.right, rect.bottom, shadow);
  CL_Display::draw_line(rect.right, rect.top, 
                        rect.right, rect.bottom, shadow);
}

void
Box::draw_button_down(const CL_Rect& rect)
{
  CL_Display::fill_rect(rect, background_sw);

  CL_Display::draw_line(rect.left, rect.bottom, 
                        rect.right, rect.bottom, highlight);
  CL_Display::draw_line(rect.right, rect.top, 
                        rect.right, rect.bottom, highlight);

  CL_Display::draw_line(rect.left, rect.top, 
                        rect.right, rect.top, shadow);
  CL_Display::draw_line(rect.left, rect.top, 
                        rect.left, rect.bottom, shadow);
}

void
Box::draw_button_neutral(const CL_Rect& rect)
{
  CL_Display::fill_rect(rect, background);
}

void
Box::draw_panel(const CL_Rect& rect)
{
  CL_Display::fill_rect(rect, background);
  CL_Display::draw_line(rect.left, rect.top, 
                        rect.right, rect.top, highlight);
  CL_Display::draw_line(rect.left, rect.top, 
                        rect.left, rect.bottom, highlight);

  CL_Display::draw_line(rect.left, rect.bottom, 
                        rect.right, rect.bottom, shadow);
  CL_Display::draw_line(rect.right, rect.top, 
                        rect.right, rect.bottom, shadow);
}

void
Box::draw_panel_down(const CL_Rect& rect)
{
  CL_Display::fill_rect(rect, background);
  CL_Display::draw_line(rect.left, rect.top, 
                        rect.right, rect.top, shadow);
  CL_Display::draw_line(rect.left, rect.top, 
                        rect.left, rect.bottom, shadow);

  CL_Display::draw_line(rect.left, rect.bottom, 
                        rect.right, rect.bottom, highlight);
  CL_Display::draw_line(rect.right, rect.top, 
                        rect.right, rect.bottom, highlight);
}

void
Box::draw_window(const CL_Rect& rect)
{
  draw_panel(CL_Rect(rect.left+1, rect.top+1, rect.right-2, rect.bottom-2));
  CL_Display::draw_rect(rect, CL_Color(0, 0, 0));
}

/* EOF */
