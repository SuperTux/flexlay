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

#include "scrollbar.hpp"

Scrollbar::Scrollbar(int min_, int max_, int page_step_, Orientation orientation_, const Rect& rect)
  : Widget(rect),
    min(min_),
    max(max_),
    page_step(page_step_),
    orientation(orientation_),
    pos(min_ + (max_ - min_)/2)
{
  
}
  
void
Scrollbar::on_mouse_motion(const MouseMotionEvent& motion)
{ 
}

void
Scrollbar::on_mouse_button(const MouseButtonEvent& button)
{
}

void
Scrollbar::draw(GraphicContext& gc)
{
  gc.fill_rect(Rect(0, 0, get_width(), get_height()), Color(200, 200, 200));

  if (orientation == HORIZONTAL)
    {
      gc.fill_rect(Rect(Point(2 + ((pos-min) * (get_rect().get_width()-4) / (max - min)),
                              2),
                        Size(page_step * (get_rect().get_width()-4) / (max - min),
                             get_rect().get_height()-4)),
                   Color(0, 0, 0));
    }
  else // VERSION
    {
      gc.fill_rect(Rect(Point(2,
                              2 + ((pos-min) * (get_rect().get_height()-4) / (max - min))),
                        Size(get_rect().get_width()-4,
                             page_step * (get_rect().get_height()-4) / (max - min))),
                   Color(0, 0, 0));
    }
}

void
Scrollbar::set_pos(int p)
{
  if (p != pos)
    {
      pos = p;
      set_dirty(true);
    }
}

/* EOF */
