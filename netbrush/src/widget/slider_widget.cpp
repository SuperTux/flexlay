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

#include <algorithm>
#include "globals.hpp"
#include "widget_manager.hpp"
#include "slider_widget.hpp"

SliderWidget::SliderWidget(int min_, int max_, int page_step_, const Rect& rect_, SliderCallback* callback_)
  : Widget(rect_), 
    min(min_),
    max(max_),
    page_step(page_step_),
    pos(min_),
    callback(callback_), 
    dragging(false)
{
}

SliderWidget::~SliderWidget()
{
  delete callback;
}

void
SliderWidget::on_mouse_motion(const MouseMotionEvent& motion)
{
  set_dirty(true);
  if (dragging)
    {
      pos = min + motion.x * (max - min) / get_rect().get_width();
      pos = std::max(min, std::min(pos, max));
      (*callback)(pos);
    }

}

void
SliderWidget::on_mouse_button(const MouseButtonEvent& button)
{
  if (button.button == 1)
    {
      if (button.state == SDL_RELEASED)
        {
          dragging = false;
          widget_manager->ungrab(this);
        }
      else if (button.state == SDL_PRESSED)
        {
          dragging = true;

          pos = min + button.x * (max - min) / get_rect().get_width();
          pos = std::max(min, std::min(pos, max));

          (*callback)(pos);

          set_dirty(true);
          widget_manager->grab(this);
        }
    }
}

void
SliderWidget::on_enter()
{
}

void
SliderWidget::on_leave()
{
}

void
SliderWidget::draw(GraphicContext& gc)
{
  Rect rect(0, 0, 
            get_rect().get_width(), get_rect().get_height());
  
  gc.fill_rect(rect,
               Color(200, 200, 200));
  
  rect.left   += 2;
  rect.top    += 10;
  rect.right  -= 2;
  rect.bottom -= 10;

  gc.fill_rect(rect, Color(100, 100, 100));

  gc.fill_rect(Rect(Point(int(get_rect().get_width() * (pos - min)/(max - min)) - 4,
                          2),
                    Size(8, get_rect().get_height() - 4)),
               Color(0, 0, 0));
}

void
SliderWidget::set_pos(int v)
{
  if (pos != v)
    {
      pos = v;
      set_dirty(true);
    }
}

/* EOF */
