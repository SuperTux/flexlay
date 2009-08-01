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
#include "drawing_context.hpp"
#include "globals.hpp"
#include "controller.hpp"
#include "color.hpp"
#include "colorpicker_tool.hpp"

ColorpickerTool::ColorpickerTool()
  : dragging(false)
{
}

ColorpickerTool::~ColorpickerTool()
{
}

void
ColorpickerTool::on_motion(const ToolMotionEvent& ev)
{
  if (dragging)
    {
      pick_color(ev.x, ev.y);
    }
}

void
ColorpickerTool::on_button_press(const ToolButtonEvent& ev)
{
  pick_color(ev.x, ev.y);
  dragging = true;
}

void
ColorpickerTool::on_button_release(const ToolButtonEvent& ev)
{
  pick_color(ev.x, ev.y);
  dragging = false;
}

void
ColorpickerTool::pick_color(int x, int y)
{
  Color color;
  if (draw_ctx->get_color(x, y, color))
    controller->set_color(color);
}

/* EOF */
