//  $Id$
//
//  Flexlay - A Generic 2D Game Editor
//  Copyright (C) 2002 Ingo Ruhnke <grumbel@gmx.de>
//
//  This program is free software; you can redistribute it and/or
//  modify it under the terms of the GNU General Public License
//  as published by the Free Software Foundation; either version 2
//  of the License, or (at your option) any later version.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//  GNU General Public License for more details.
//
//  You should have received a copy of the GNU General Public License
//  along with this program; if not, write to the Free Software
//  Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.

#include <ClanLib/Display/display.h>
#include <ClanLib/Display/keys.h>
#include "scrollbar.hxx"

class ScrollbarImpl
{
public:
  std::vector<CL_Slot> slots;

  ScrollbarImpl(Scrollbar* parent_) : parent(parent_) {}

  Scrollbar* parent;
  float min;
  float max;
  float pagesize;
  float pos;
  Scrollbar::Orientation orientation;

  CL_Signal_v1<float> on_scrollbar_move;

  float old_pos;
  
  bool pressed;
  CL_Point click_pos;
  
  void draw();
  void on_mouse_up(const CL_InputEvent& event);
  void on_mouse_down(const CL_InputEvent& event);
  void on_mouse_move(const CL_InputEvent& event);
};

Scrollbar::Scrollbar(const CL_Rect& rect, Orientation orientation, CL_Component* parent)
  : CL_Component(rect, parent),
    impl(new ScrollbarImpl(this))
{
  impl->min = 0;
  impl->max = 100;
  impl->pagesize = 10;
  impl->pos  = 0;
  impl->pressed = false;
  impl->orientation = orientation;

  impl->slots.push_back(sig_paint().connect(impl.get(), &ScrollbarImpl::draw));

  impl->slots.push_back(sig_mouse_down().connect(impl.get(), &ScrollbarImpl::on_mouse_down));
  impl->slots.push_back(sig_mouse_up().connect(impl.get(), &ScrollbarImpl::on_mouse_up));
  impl->slots.push_back(sig_mouse_move().connect(impl.get(), &ScrollbarImpl::on_mouse_move));
}
  
void
Scrollbar::set_range(float min, float max)
{
  impl->min = min;
  impl->max = max;
}

void
Scrollbar::set_pagesize(float size)
{
  impl->pagesize = size;  
}

void
Scrollbar::set_pos(float pos)
{
  impl->pos = pos;
}

void 
ScrollbarImpl::draw()
{
  CL_Display::push_translate(parent->get_screen_x(), parent->get_screen_y());

  CL_Rect rect = CL_Rect(CL_Point(0, 0), 
                         CL_Size(parent->get_width()-1,
                                 parent->get_height()-1));
  CL_Display::fill_rect(rect,
                        CL_Color(255, 255, 255));

  if (orientation == Scrollbar::HORIZONTAL)
    {
      float scale = parent->get_width()/(max - min);
      CL_Display::fill_rect(CL_Rect(CL_Point(int((pos-min-(pagesize/2)) * scale), 2), 
                                    CL_Size(int(pagesize*scale), 
                                            parent->get_height()-5)),
                            CL_Color(0, 0, 0));
    }
  else if (orientation == Scrollbar::VERTICAL)
    {
      float scale = parent->get_height()/(max - min);
      CL_Display::fill_rect(CL_Rect(CL_Point(2, int((pos-min-(pagesize/2)) * scale)), 
                                    CL_Size(parent->get_width()-5,
                                            int(pagesize*scale))),
                            CL_Color(0, 0, 0));     
    }

  CL_Display::draw_rect(rect,
                        CL_Color(155, 155, 155));

  CL_Display::pop_modelview();
}

void
ScrollbarImpl::on_mouse_up(const CL_InputEvent& event)
{
  if (event.id == CL_MOUSE_LEFT)
    {
      pressed = false;
      parent->release_mouse();
    }
}

void
ScrollbarImpl::on_mouse_down(const CL_InputEvent& event)
{
  if (event.id == CL_MOUSE_LEFT)
    {
      pressed   = true;
      click_pos = event.mouse_pos;
      
      parent->capture_mouse();

      float scale = ((orientation == Scrollbar::VERTICAL)
                     ? parent->get_height() : parent->get_width())/(max - min);
      old_pos = pos * scale;

      click_pos.x += parent->get_position().left;
      click_pos.y += parent->get_position().top;
    }
}

void
ScrollbarImpl::on_mouse_move(const CL_InputEvent& event)
{
  if(pressed)
    {
      CL_Rect rect = parent->get_position();
      
      float scale = ((orientation == Scrollbar::VERTICAL)
                     ? parent->get_height() : parent->get_width())/(max - min);
      
      if (orientation == Scrollbar::VERTICAL)
        {
          pos = (old_pos - (click_pos.y - (rect.top + event.mouse_pos.y)))/scale;
        }
      else if (orientation == Scrollbar::HORIZONTAL)
        {
          pos = (old_pos - (click_pos.x - (rect.left + event.mouse_pos.x)))/scale;
        }
      on_scrollbar_move(pos);
    }
}

CL_Signal_v1<float>&
Scrollbar::sig_scrollbar_move()
{
  return impl->on_scrollbar_move;
}

/* EOF */
