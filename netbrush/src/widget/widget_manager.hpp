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

#ifndef HEADER_WIDGET_MANAGER_HPP
#define HEADER_WIDGET_MANAGER_HPP

#include <list>
#include "SDL.h"

class Point;
class MouseMotionEvent;
class MouseButtonEvent;
class PenEvent;

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
  void on_pen_motion(const PenEvent& pen);

  void grab(Widget* widget);
  void ungrab(Widget* widget);

  void add(Widget* widget);

  void update();

private:
  Widget* get_widget(const Point& p) const;

  WidgetManager (const WidgetManager&);
  WidgetManager& operator= (const WidgetManager&);
};

#endif

/* EOF */
