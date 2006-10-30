/*  $Id$
**   __      __ __             ___        __   __ __   __
**  /  \    /  \__| ____    __| _/_______/  |_|__|  | |  |   ____
**  \   \/\/   /  |/    \  / __ |/  ___/\   __\  |  | |  | _/ __ \
**   \        /|  |   |  \/ /_/ |\___ \  |  | |  |  |_|  |_\  ___/
**    \__/\  / |__|___|  /\____ /____  > |__| |__|____/____/\___  >
**         \/          \/      \/    \/                         \/
**  Copyright (C) 2005 Ingo Ruhnke <grumbel@gmx.de>
**
**  This program is free software; you can redistribute it and/or
**  modify it under the terms of the GNU General Public License
**  as published by the Free Software Foundation; either version 2
**  of the License, or (at your option) any later version.
**
**  This program is distributed in the hope that it will be useful,
**  but WITHOUT ANY WARRANTY; without even the implied warranty of
**  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
**  GNU General Public License for more details.
** 
**  You should have received a copy of the GNU General Public License
**  along with this program; if not, write to the Free Software
**  Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
**  02111-1307, USA.
*/

#include <iostream>
#include <sstream>
#include "video.hpp"
#include "drawing_context.hpp"
#include "drawing_parameter.hpp"
#include "stroke_buffer.hpp"
#include "widget/widget_manager.hpp"
#include "globals.hpp"
#include "server_connection.hpp"
#include "widget/scrollbar.hpp"
#include "tool.hpp"
#include "airbrush_tool.hpp"
#include "scroll_tool.hpp"
#include "colorpicker_tool.hpp"
#include "region_tool.hpp"
#include "rect_tool.hpp"
#include "screen_buffer.hpp"

ScreenBuffer::ScreenBuffer(const Rect& rect)
  : Widget(rect),
    complete_refresh(false),
    scroll_offset_x(0),
    scroll_offset_y(0)
{
  tools.push_back(new AirbrushTool());
  tools.push_back(new ScrollTool());
  tools.push_back(new ColorpickerTool());
  tools.push_back(new RectTool());
  tools.push_back(new RegionTool());
}

ScreenBuffer::~ScreenBuffer()
{
  //  SDL_FreeSurface(buffer);
}

void
ScreenBuffer::draw(SDL_Surface* target)
{
  //SDL_SetClipRect(target, &rect);

  int trans_x = get_rect().left + scroll_offset_x;
  int trans_y = get_rect().top  + scroll_offset_y;

  // Transform dirty_region into screen space
  dirty_region.left   += trans_x;
  dirty_region.top    += trans_y;
  dirty_region.right  += trans_x;
  dirty_region.bottom += trans_y;
  
  if (1)
    {
      dirty_region.left   = std::max(get_rect().left,   dirty_region.left);
      dirty_region.top    = std::max(get_rect().top,    dirty_region.top);
      dirty_region.right  = std::min(get_rect().right,  dirty_region.right);
      dirty_region.bottom = std::min(get_rect().bottom, dirty_region.bottom);
    }

  if (0)
    std::cout << "Updating screen: "
              << dirty_region.left  << " "
              << dirty_region.top   << " "
              << dirty_region.right << " "
              << dirty_region.bottom 
              << std::endl;

  // FIXME: Should go elsewhere
  horizontal_scrollbar->set_pos(-scroll_offset_x);
  vertical_scrollbar->set_pos(-scroll_offset_y);

  if (complete_refresh)
    { 
      Uint32 black = SDL_MapRGB(target->format, 200, 200, 200);
      Uint32 white = SDL_MapRGB(target->format, 100, 100, 100);

      SDL_Rect r;     
      for(int y = get_rect().top; y < get_rect().bottom; y += 32)
        for(int x = get_rect().left; x < get_rect().right; x += 32)
          {
            r.x = x;
            r.y = y;
            
            r.w = 32;
            r.h = 32;
            
            if (((x / 32) % 2) ^ ((y / 32) % 2))
              SDL_FillRect(target, &r, black);
            else
              SDL_FillRect(target, &r, white);
          }
    }

  // check for invalid dirty_regions (ie. canvas is completly outside of the view)
  if (dirty_region.left < dirty_region.right &&
      dirty_region.top  <  dirty_region.bottom)
    {
      draw_ctx->draw(target, dirty_region, trans_x, trans_y);
      if (!complete_refresh)
        stroke_buffer->draw(target, dirty_region, trans_x, trans_y);
  
      if (complete_refresh)
        { 
          SDL_Rect r;
          r.x = get_rect().left;
          r.y = get_rect().top;
          r.w = get_rect().get_width();
          r.h = get_rect().get_height();
            
          SDL_UpdateRect(target, r.x, r.y, r.w, r.h);
        }
      else
        {
          SDL_UpdateRect(target, 
                         dirty_region.left,        dirty_region.top, 
                         dirty_region.get_width(), dirty_region.get_height());
        }
    }
  else 
    {
      if (complete_refresh)
        { 
          SDL_Rect r;
          r.x = get_rect().left;
          r.y = get_rect().top;
          r.w = get_rect().get_width();
          r.h = get_rect().get_height();
            
          SDL_UpdateRect(target, r.x, r.y, r.w, r.h);
        }
    }

  if (0) 
    std::cout << "Updating done" << std::endl;

  complete_refresh = false;
}

void
ScreenBuffer::mark_dirty(const Rect& region)
{
  mark_dirty(region.left, region.top, region.get_width(), region.get_height());
}

void
ScreenBuffer::mark_dirty(int x, int y, int w, int h)
{
  if (x < 0)
    x = 0;

  if (y < 0)
    y = 0;
  
  // FIXME: This must be drawable size, not screen size
  if (x + w > draw_ctx->get_width())
    w = draw_ctx->get_width() - x;

  if (y + h > draw_ctx->get_height())
    h = draw_ctx->get_height() - y;

  //std::cout << "Dirty: " << x << " " << y << " " << w << " " << h << std::endl;

  if (is_dirty())
    {
      int x1 = dirty_region.left;
      int y1 = dirty_region.top;
      int x2 = dirty_region.right;
      int y2 = dirty_region.bottom;

      dirty_region.left = std::min(x, x1);
      dirty_region.top  = std::min(y, y1);
      
      dirty_region.right  = std::max(x+w, x2);
      dirty_region.bottom = std::max(y+h, y2);
    }
  else
    {
      dirty_region.left   = x;
      dirty_region.top    = y;
      dirty_region.right  = x + w;
      dirty_region.bottom = y + h;
      set_dirty(true);
    }
}

void
ScreenBuffer::on_mouse_motion(const MouseMotionEvent& motion)
{
  ToolMotionEvent tool_motion;

  tool_motion.x = motion.x - scroll_offset_x;
  tool_motion.y = motion.y - scroll_offset_y;

  tool_motion.screen = Point(motion.x, motion.y);
  
  for(Tools::iterator i = tools.begin(); i != tools.end(); ++i)
    (*i)->on_motion(tool_motion);
}

void
ScreenBuffer::on_mouse_button(const MouseButtonEvent& button)
{
  ToolButtonEvent tool_button;

  tool_button.screen = Point(button.x, button.y);

  tool_button.x = button.x - scroll_offset_x;
  tool_button.y = button.y - scroll_offset_y;
  
  if (button.button >= 1 && button.button <= int(tools.size()))
    {
      if (button.state == SDL_PRESSED)
        tools[button.button-1]->on_button_press(tool_button);
      else if (button.state == SDL_RELEASED)
        tools[button.button-1]->on_button_release(tool_button);
    }
}

void
ScreenBuffer::force_full_refresh()
{
  Rect r(0, 0, screen_buffer->get_rect().get_width(), screen_buffer->get_rect().get_height());
  r.left   -= scroll_offset_x;
  r.right  -= scroll_offset_x;
  r.top    -= scroll_offset_y;
  r.bottom -= scroll_offset_y;
  screen_buffer->mark_dirty(r);
  complete_refresh = true;
  set_dirty(true);
}

void
ScreenBuffer::move_to(const Point& p)
{
  scroll_offset_x = get_rect().get_width()/2  - p.x;
  scroll_offset_y = get_rect().get_height()/2 - p.y;

  force_full_refresh();
}

Point
ScreenBuffer::get_pos()
{
  return Point(get_rect().get_width()/2  - scroll_offset_x,
               get_rect().get_height()/2 - scroll_offset_y);
}

/* EOF */
