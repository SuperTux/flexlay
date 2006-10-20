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

#ifndef HEADER_WIDGET_MANAGER_HPP
#define HEADER_WIDGET_MANAGER_HPP

#include <list>
#include "SDL.h"

class MouseMotionEvent;
class MouseButtonEvent;

class Widget;

/** */
class WidgetManager
{
private:
  typedef std::list<Widget*> Widgets;
  Widgets widgets;
  Widget* grabbed_widget;
  Widget* focused_widget;
public:
  WidgetManager();
  ~WidgetManager();
  
  void on_mouse_motion(const MouseMotionEvent& motion);
  void on_mouse_button(const MouseButtonEvent& button);

  void grab(Widget* widget);
  void ungrab(Widget* widget);

  void add(Widget* widget);

  void update();

private:
  WidgetManager (const WidgetManager&);
  WidgetManager& operator= (const WidgetManager&);
};

#endif

/* EOF */
