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
#include "screen_buffer.hpp"

void send_stroke(const Stroke& stroke, DrawingParameter* param)
{
  if (tcpsock)
    {
      const Stroke::Dabs& dabs = stroke.get_dabs();

      std::stringstream str;
      if (param->get_brush().empty())
        str << "set_generic_brush " 
            << param->generic_brush.shape << " "
            << param->generic_brush.radius << " "
            << param->generic_brush.spikes << " "
            << param->generic_brush.hardness << " "
            << param->generic_brush.aspect_ratio << " "
            << param->generic_brush.angle << " "
            << std::endl;
      else
          str << "set_brush " << param->get_brush() << std::endl;
      str << "set_opacity " << int(param->opacity) << std::endl;
      str << "set_color "
          << int(param->color.r) << " " 
          << int(param->color.g) << " " 
          << int(param->color.b) << std::endl;
      str << "stroke_begin" << std::endl;
      for(Stroke::Dabs::const_iterator i = dabs.begin(); i != dabs.end(); ++i)
        {
          str << "dab " << i->time << " " << i->pos.x << " " << i->pos.y  << std::endl;
        }
      str << "stroke_end" << std::endl;
      
      std::string line = str.str();

      int result = SDLNet_TCP_Send(tcpsock, const_cast<char*>(line.c_str()), line.length());
      if(result < int(line.length()))
        {
          printf( "SDLNet_TCP_Send: %s\n", SDLNet_GetError() );
          // It may be good to disconnect sock because it is likely invalid now.
        }     
    }
}

ScreenBuffer::ScreenBuffer(const Rect& rect)
  : Widget(rect),
    scroll_offset_x(0),
    scroll_offset_y(0),
    old_scroll_offset_x(0),
    old_scroll_offset_y(0),
    click_pos_x(0),
    click_pos_y(0),
    scrolling(false)
{
#if 0 
  // Create drawable
  Uint32 rmask, gmask, bmask, amask;

  /* SDL interprets each pixel as a 32-bit number, so our masks must depend
     on the endianness (byte order) of the machine */
#if SDL_BYTEORDER == SDL_BIG_ENDIAN
  rmask = 0xff000000;
  gmask = 0x00ff0000;
  bmask = 0x0000ff00;
  amask = 0; //0x000000ff;
#else
  rmask = 0x000000ff;
  gmask = 0x0000ff00;
  bmask = 0x00ff0000;
  amask = 0; //0xff000000;
#endif

  buffer = SDL_CreateRGBSurface(SDL_SWSURFACE, w, h, 24,
                                rmask, gmask, bmask, amask);

  if(buffer == NULL) 
    {
      fprintf(stderr, "CreateRGBSurface failed: %s\n", SDL_GetError());
      exit(1);
    }
#endif 
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

  draw_ctx->draw(target, dirty_region, trans_x, trans_y);

  stroke_buffer->draw(target, dirty_region, trans_x, trans_y);
  
  SDL_UpdateRect(target, 
                 dirty_region.left,        dirty_region.top, 
                 dirty_region.get_width(), dirty_region.get_height());

  if (0) 
    std::cout << "Updating done" << std::endl;
}

void
ScreenBuffer::mark_dirty(SDL_Rect* region)
{
  mark_dirty(region->x, region->y, region->w, region->h);
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
  if (x + w > screen->w)
    w = screen->w - x;

  if (y + h > screen->h)
    h = screen->h - y;

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
  //satd::cout << motion.x << " " << motion.y << std::endl;
  if (current_stroke)
    {
      current_stroke->add_dab(Dab(motion.x - scroll_offset_x, motion.y - scroll_offset_y));
      stroke_buffer->add_dab(Dab(motion.x - scroll_offset_x, motion.y - scroll_offset_y));

      // sync
      Rect rect = current_stroke->get_bounding_rect(); 
              
      // calculate bounding rectangle by taking brush thickness into account
      rect.left -= client_draw_param->thickness()/2;
      rect.top  -= client_draw_param->thickness()/2;

      rect.right  += client_draw_param->thickness()/2;
      rect.bottom += client_draw_param->thickness()/2;
                  
      screen_buffer->mark_dirty(rect);
    }

  if (scrolling)
    {
      scroll_offset_x = old_scroll_offset_x + (motion.x - click_pos_x);
      scroll_offset_y = old_scroll_offset_y + (motion.y - click_pos_y);
      mark_dirty(get_rect());
      //std::cout << "Scrolling: " << scroll_offset_x << " " << scroll_offset_y << std::endl;
    } 
}

void
ScreenBuffer::on_mouse_button(const MouseButtonEvent& button)
{
  if (button.state == SDL_RELEASED)
    {
      if (button.button == 1)
        {
          if (current_stroke)
            {
              widget_manager->ungrab(this);

              if (!tcpsock)
                { // FIXME: Might not work, not really maintained
                  draw_ctx->draw_stroke(*current_stroke, client_draw_param);
                }
              else
                {
                  stroke_buffer->clear();
                  send_stroke(*current_stroke, client_draw_param);
                }

              current_stroke = 0;
            }
        }
      else if (button.button == 2)
        {
          scroll_offset_x = old_scroll_offset_x + (button.x - click_pos_x);
          scroll_offset_y = old_scroll_offset_y + (button.y - click_pos_y);
          scrolling = false;
        }
    }  
  else if (button.state == SDL_PRESSED)
    {
      if (button.button == 1)
        {
          widget_manager->grab(this);
          current_stroke = new Stroke();

          current_stroke->add_dab(Dab(button.x - scroll_offset_x, button.y - scroll_offset_y));
          stroke_buffer->add_dab(Dab(button.x - scroll_offset_x, button.y - scroll_offset_y));

          // FIXME: First dab is lost
        }
      else if (button.button == 2)
        {
          click_pos_x = button.x;
          click_pos_y = button.y;

          old_scroll_offset_x = scroll_offset_x;
          old_scroll_offset_y = scroll_offset_y;

          scrolling = true;
        }
    }
}

/* EOF */
