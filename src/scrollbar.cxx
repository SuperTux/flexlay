//  $Id$
//
//  Pingus - A free Lemmings clone
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
  
  void draw();
};

Scrollbar::Scrollbar(const CL_Rect& rect, CL_Component* parent)
  : CL_Component(rect, parent),
    impl(new ScrollbarImpl(this))
{
  impl->min = 0;
  impl->max = 100;
  impl->pagesize = 10;
  impl->pos  = 0;

  impl->slots.push_back(sig_paint().connect(impl.get(), &ScrollbarImpl::draw));
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
  CL_Rect rect = CL_Rect(CL_Point(0, 0), 
                         CL_Size(parent->get_width()-1,
                                 parent->get_height()-1));
  CL_Display::fill_rect(rect,
                        CL_Color(255, 255, 255));
  CL_Display::draw_rect(rect,
                        CL_Color(155, 155, 155));

  float scale = parent->get_height()/(max - min);

  CL_Display::fill_rect(CL_Rect(CL_Point(2, int((pos-min-(pagesize/2)) * scale)), 
                                CL_Size(parent->get_width()-5,
                                        int(pagesize*scale))),
                        CL_Color(0, 0, 0));
}

/* EOF */
