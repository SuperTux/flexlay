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

#include "globals.hpp"
#include "screen_buffer.hpp"
#include "math/rect.hpp"
#include "scroll_tool.hpp"

ScrollTool::ScrollTool()
  : scrolling(false),
    old_pos(0,0),
    click_pos(0,0)
{
}

ScrollTool::~ScrollTool()
{
}

void
ScrollTool::on_motion(const ToolMotionEvent& ev)
{
  if (scrolling)
    {
      screen_buffer->move_to(Point(old_pos.x - (ev.screen.x - click_pos.x),
                                   old_pos.y - (ev.screen.y - click_pos.y)));
    }
}

void
ScrollTool::on_button_press(const ToolButtonEvent& ev)
{
  click_pos.x = ev.screen.x;
  click_pos.y = ev.screen.y;

  old_pos = screen_buffer->get_pos();

  scrolling = true;
}

void
ScrollTool::on_button_release(const ToolButtonEvent& ev)
{ 
  // FIXME: grab
  screen_buffer->move_to(Point(old_pos.x - (ev.screen.x - click_pos.x),
                               old_pos.y - (ev.screen.y - click_pos.y)));

  scrolling = false;
}

/* EOF */
