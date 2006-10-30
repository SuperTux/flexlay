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

#include <sstream>
#include "SDL_gfx/SDL_gfxPrimitives.h"
#include "globals.hpp"
#include "server_connection.hpp"
#include "controller.hpp"
#include "screen_buffer.hpp"
#include "drawing_parameter.hpp"
#include "rect_tool.hpp"

RectTool::RectTool()
  : dragging(false)
{
}

RectTool::~RectTool()
{

}

void
RectTool::on_motion(const ToolMotionEvent& ev)
{
  if (dragging)
    {
      rect.right  = ev.x;
      rect.bottom = ev.y;
     
      screen_buffer->force_full_refresh();
    }
}

void
RectTool::on_button_press(const ToolButtonEvent& ev)
{
  dragging = true;
  rect.left = ev.x;
  rect.top  = ev.y;
}

void
RectTool::on_button_release(const ToolButtonEvent& ev)
{
  dragging = false;
  rect.right  = ev.x;
  rect.bottom = ev.y;
  
  rect.normalize();
  
  std::ostringstream str;
  str << "set_color "
      << int(client_draw_param->color.r) << " " 
      << int(client_draw_param->color.g) << " " 
      << int(client_draw_param->color.b) << std::endl;

  str << "set_opacity " << int(client_draw_param->opacity) << std::endl;

  str << "fill_rect "
      << rect.left  << " " << rect.top << " " 
      << rect.right << " " << rect.bottom << " "
      << std::endl;
  server->send(str.str());
}

void
RectTool::draw(SDL_Surface* target, const Rect& rect__, int x_of, int y_of)
{
  Rect rect_ = rect;
  rect_.normalize();
  if (dragging)
    boxRGBA(target,
            int(rect_.left   + x_of), 
            int(rect_.top    + y_of), 
            int(rect_.right  + x_of), 
            int(rect_.bottom + y_of), 
                   
            client_draw_param->color.r,
            client_draw_param->color.g,
            client_draw_param->color.b,
            client_draw_param->opacity);

}

/* EOF */
