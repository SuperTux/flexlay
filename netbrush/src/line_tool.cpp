/*            _   ___              _   
**   _ _  ___| |_| _ )_ _ _  _ _ _| |_ 
**  | ' \/ -_)  _| _ \ '_| || (_-<|   |
**  |_||_\___|\__|___/_|  \_,_/__/|_|_|
**  netBrush - Copyright (C) 2006 Ingo Ruhnke <grumbel@gmx.de>
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
#include "drawing_parameter.hpp"
#include "screen_buffer.hpp"
#include "server_connection.hpp"
#include "drawing_parameter.hpp"
#include "line_tool.hpp"

LineTool::LineTool()
  : dragging(false)
{
}

LineTool::~LineTool()
{
}

void
LineTool::on_motion(const ToolMotionEvent& ev)
{
  if (dragging)
    {
      p2 = Point(ev.x, ev.y);
      screen_buffer->force_full_refresh();
    }
}

void
LineTool::on_button_press(const ToolButtonEvent& ev)
{
  dragging = true;
  p1 = Point(ev.x, ev.y);
}

void
LineTool::on_button_release(const ToolButtonEvent& ev)
{
  if (dragging)
    {
      dragging = false;
      p2 = Point(ev.x, ev.y);

      // FIXME: Send line drawing request

      std::ostringstream str;
      str << "set_color "
          << int(client_draw_param->color.r) << " " 
          << int(client_draw_param->color.g) << " " 
          << int(client_draw_param->color.b) << std::endl;

      str << "set_opacity " << int(client_draw_param->opacity) << std::endl;

      str << "draw_line "
          << p1.x  << " " << p1.y << " " 
          << p2.x  << " " << p2.y << " " 
          << std::endl;
      server->send(str.str());
    }
}

void
LineTool::draw(SDL_Surface* target, const Rect& rect, int x_of, int y_of)
{
  if (dragging)
    {
      lineRGBA(target, 
               p1.x + x_of,  p1.y + y_of,
               p2.x + x_of,  p2.y + y_of,
               client_draw_param->color.r, client_draw_param->color.g, client_draw_param->color.b, 
               client_draw_param->opacity);
    }
}

/* EOF */
