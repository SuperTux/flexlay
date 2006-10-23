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

#include "globals.hpp"
#include "stroke.hpp"
#include "stroke_buffer.hpp"
#include "screen_buffer.hpp"
#include "server_connection.hpp"
#include "drawing_parameter.hpp"
#include "widget/widget_manager.hpp"
#include "airbrush_tool.hpp"

AirbrushTool::AirbrushTool()
{
}

AirbrushTool::~AirbrushTool()
{
}

void
AirbrushTool::on_motion(const ToolMotionEvent& ev)
{
  if (current_stroke)
    {
      current_stroke->add_dab(Dab(ev.x, ev.y));
      stroke_buffer->add_dab(Dab(ev.x, ev.y));

      // sync
      Rect rect = current_stroke->get_bounding_rect(); 
              
      // calculate bounding rectangle by taking brush thickness into account
      // FIXME: Handle x/y thickness independetly to get a smaller clip rect
      rect.left -= client_draw_param->thickness()/2;
      rect.top  -= client_draw_param->thickness()/2;

      rect.right  += client_draw_param->thickness()/2;
      rect.bottom += client_draw_param->thickness()/2;
                  
      screen_buffer->mark_dirty(rect);
    } 
}

void
AirbrushTool::on_button_press(const ToolButtonEvent& ev)
{
  widget_manager->grab(screen_buffer); // FIXME: ugly out of place widget class abuse

  // FIXME: activate this and test: delete current_stroke;

  current_stroke = new Stroke();

  current_stroke->add_dab(Dab(ev.x, ev.y));
  stroke_buffer->add_dab(Dab(ev.x, ev.y));

  // FIXME: First dab is lost
}

void
AirbrushTool::on_button_release(const ToolButtonEvent& ev)
{
  if (current_stroke)
    {
      widget_manager->ungrab(screen_buffer); // FIXME: ugly out of place widget class abuse

      stroke_buffer->clear();
      server->send_stroke(*current_stroke, client_draw_param);

      current_stroke = 0;
    }
}

/* EOF */
