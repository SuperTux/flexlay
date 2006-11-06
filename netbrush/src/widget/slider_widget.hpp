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

#ifndef HEADER_SLIDER_WIDGET_HPP
#define HEADER_SLIDER_WIDGET_HPP

#include "widget.hpp"

/** */
class SliderCallback
{
public:
  SliderCallback() {}
  virtual ~SliderCallback() {}
  virtual void operator()(int) =0;

private:
  SliderCallback (const SliderCallback&);
  SliderCallback& operator= (const SliderCallback&);
};

/** */
class SliderWidget : public Widget
{
private:
  int min;
  int max;
  int page_step;
  int pos;

  SliderCallback* callback;
  bool  dragging;
  
public:
  SliderWidget(int min, int max, int page_step, const Rect& rect_, SliderCallback* callback);
  ~SliderWidget();

  void on_mouse_motion(const MouseMotionEvent& motion);
  void on_mouse_button(const MouseButtonEvent& button);

  void on_enter();
  void on_leave();

  void draw(GraphicContext& gc);

  void set_pos(int v);

private:
  SliderWidget (const SliderWidget&);
  SliderWidget& operator= (const SliderWidget&);
};

#endif

/* EOF */
