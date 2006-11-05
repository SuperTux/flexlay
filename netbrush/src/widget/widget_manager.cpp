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

void
WidgetManager::on_pen_motion(const PenEvent& pen)
{
  // FIXME: Hack, should to normal widget handling instead
  screen_buffer->on_pen_motion(pen);
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
