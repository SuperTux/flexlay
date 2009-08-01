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

#include <assert.h>
#include <iostream>
#include "../video.hpp"
#include "widget.hpp"
#include "events.hpp"
#include "../globals.hpp"
#include "../screen_buffer.hpp"
#include "surface_graphic_context.hpp"
#include "widget_manager.hpp"

WidgetManager::WidgetManager()
  : grabbed_widget(0), focused_widget(0)
{
  
}

WidgetManager::~WidgetManager()
{
  for(Widgets::iterator i = widgets.begin(); i != widgets.end(); ++i)
    {
      delete *i;
    }
}

void
WidgetManager::add(Widget* widget)
{
  widget->set_dirty(true);
  widgets.push_back(widget);
}

void
WidgetManager::on_mouse_motion(const MouseMotionEvent& motion)
{
  if (grabbed_widget)
    {
      MouseMotionEvent trans_motion(motion);

      trans_motion.x -= grabbed_widget->get_rect().left;
      trans_motion.y -= grabbed_widget->get_rect().top;

      grabbed_widget->on_mouse_motion(trans_motion);
    }
  else
    {
      Widget* old_focused_widget = focused_widget;
      
      focused_widget = 0;

      for(Widgets::iterator i = widgets.begin(); i != widgets.end(); ++i)
        {
          if ((*i)->get_rect().is_inside(Point(motion.x, motion.y)))
            {
              MouseMotionEvent trans_motion(motion);

              trans_motion.x -= (*i)->get_rect().left;
              trans_motion.y -= (*i)->get_rect().top;

              if (!grabbed_widget)
                focused_widget = (*i);
              
              (*i)->on_mouse_motion(trans_motion);
              break;
            }
        }

      if (!grabbed_widget)
        {
          if (old_focused_widget != focused_widget)
            {
              if (old_focused_widget)
                old_focused_widget->on_leave();
         
              if (focused_widget)
                focused_widget->on_enter();
            }
        }
      else
        {
          focused_widget = grabbed_widget;
        }
    }
}

void
WidgetManager::on_mouse_button(const MouseButtonEvent& button)
{
  if (grabbed_widget)
    {
      MouseButtonEvent trans_button(button);

      trans_button.x -= grabbed_widget->get_rect().left;
      trans_button.y -= grabbed_widget->get_rect().top;

      grabbed_widget->on_mouse_button(trans_button);
    }
  else
    {
      for(Widgets::iterator i = widgets.begin(); i != widgets.end(); ++i)
        {
          if ((*i)->get_rect().is_inside(Point(button.x, button.y)))
            {
              MouseButtonEvent trans_button(button);
              trans_button.x -= (*i)->get_rect().left;
              trans_button.y -= (*i)->get_rect().top;

              (*i)->on_mouse_button(trans_button);
              break;
            }
        }  
    }
}

Widget*
WidgetManager::get_widget(const Point& p) const
{
  for(Widgets::const_iterator i = widgets.begin(); i != widgets.end(); ++i)
    {
      if ((*i)->get_rect().is_inside(Point(p.x, p.y)))
        return (*i);
    }
  return 0;
}

void
WidgetManager::on_pen_motion(const PenEvent& pen)
{
  // FIXME: Hack, should to normal widget handling instead
  PenEvent new_pen = pen;

  Widget* widget = get_widget(Point(int(new_pen.x), int(new_pen.y)));
  if (widget)
    {
      new_pen.x -= screen_buffer->get_rect().left;
      new_pen.y -= widget->get_rect().top;

      widget->on_pen_motion(new_pen);
    }
}

void
WidgetManager::update()
{
  for(Widgets::reverse_iterator i = widgets.rbegin(); i != widgets.rend(); ++i)
    {
      if ((*i)->is_dirty())
        {
          SDL_Rect clip_rect;
          clip_rect.x = (*i)->get_rect().left;
          clip_rect.y = (*i)->get_rect().top;
          clip_rect.w = (*i)->get_rect().get_width();
          clip_rect.h = (*i)->get_rect().get_height();

          SDL_SetClipRect(screen, &clip_rect);

          SurfaceGraphicContext gc(screen, (*i)->get_rect());
          (*i)->draw(gc);

          SDL_SetClipRect(screen, NULL);

          // FIXME: might conflict with Screenbuffers update procedure
          if ((*i)->do_update())
            {
              if (0)
                std::cout << "\nWidgetManager: update: "
                          << (*i)->get_rect().left << " " << (*i)->get_rect().top << " "
                          << (*i)->get_rect().get_width() << " " << (*i)->get_rect().get_height() << std::endl;

              SDL_UpdateRect(screen, 
                             (*i)->get_rect().left,        (*i)->get_rect().top,
                             (*i)->get_rect().get_width(), (*i)->get_rect().get_height());
            }

          (*i)->set_dirty(false);
        }
    }  
}

void
WidgetManager::grab(Widget* widget)
{
  assert(grabbed_widget == 0);
  grabbed_widget = widget;
}

void
WidgetManager::ungrab(Widget* widget)
{
  //assert(grabbed_widget == widget);
  grabbed_widget = 0;
}

/* EOF */
