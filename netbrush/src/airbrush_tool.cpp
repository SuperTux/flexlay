/*            _   ___              _   
**   _ _  ___| |_| _ )_ _ _  _ _ _| |_ 
**  | ' \/ -_)  _| _ \ '_| || (_-<|   |
**  |_||_\___|\__|___/_|  \_,_/__/|_|_|
**  netBrush - Copyright (C) 2006 Ingo Ruhnke <grumbel@gmx.de>
**
**  This program is free software: you can redistribute it and/or modify
**  it under the terms of the GNU General Public License as published by
**  the Free Software Foundation, either version 3 of the License, or
**  (at your option) any later version.
**  
**  This program is distributed in the hope that it will be useful,
**  but WITHOUT ANY WARRANTY; without even the implied warranty of
**  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
**  GNU General Public License for more details.
**  
**  You should have received a copy of the GNU General Public License
**  along with this program.  If not, see <http://www.gnu.org/licenses/>.
*/

#include <iostream>
#include "globals.hpp"
#include "stroke.hpp"
#include "stroke_buffer.hpp"
#include "screen_buffer.hpp"
#include "server_connection.hpp"
#include "drawing_parameter.hpp"
#include "widget/widget_manager.hpp"
#include "airbrush_tool.hpp"

AirbrushTool::AirbrushTool()
  : current_stroke(0),
    pen_active(false)
{
}

AirbrushTool::~AirbrushTool()
{
}

void
AirbrushTool::on_motion(const ToolMotionEvent& ev)
{
  if (pen_active) return;

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
AirbrushTool::on_pen_motion(const PenEvent& pen)
{
  if (0 && pen_active)
    printf("x: %1.5f y: %1.5f pressure: %1.5f x_tilt: %2.5f y_tilt: %2.5f\n",
           pen.x, pen.y, pen.pressure, pen.x_tilt, pen.y_tilt);

  if (pen.pressure > 0)
    {
      if (!current_stroke)
        {
          current_stroke = new Stroke();
          pen_active = true;
        }      

      float x = pen.x;
      float y = pen.y;

      current_stroke->add_dab(Dab(x, y, pen.pressure));
      stroke_buffer->add_dab(Dab(x, y, pen.pressure));

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
  else if (current_stroke)
    {
      stroke_buffer->clear();
      server->send_stroke(*current_stroke, client_draw_param);

      delete current_stroke;
      current_stroke = 0;
      pen_active = false;
    }
}

void
AirbrushTool::on_button_press(const ToolButtonEvent& ev)
{
  if (pen_active) return;

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
  if (pen_active) return;

  if (current_stroke)
    {
      widget_manager->ungrab(screen_buffer); // FIXME: ugly out of place widget class abuse

      stroke_buffer->clear();
      server->send_stroke(*current_stroke, client_draw_param);

      delete current_stroke;
      current_stroke = 0;
    }
}

/* EOF */
