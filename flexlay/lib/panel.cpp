//  Flexlay - A Generic 2D Game Editor
//  Copyright (C) 2002 Ingo Ruhnke <grumbel@gmx.de>
//
//  This program is free software: you can redistribute it and/or modify
//  it under the terms of the GNU General Public License as published by
//  the Free Software Foundation, either version 3 of the License, or
//  (at your option) any later version.
//  
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//  GNU General Public License for more details.
//  
//  You should have received a copy of the GNU General Public License
//  along with this program.  If not, see <http://www.gnu.org/licenses/>.

#include <vector>
#include <ClanLib/Signals/slot.h>
#include <ClanLib/Display/display.h>
#include "box.hpp"
#include "panel.hpp"

class PanelImpl
{
public:
  std::vector<CL_Slot> slots;
  CL_Component* parent;

  void draw();
};

Panel::Panel(const CL_Rect& rect, CL_Component* parent)
  : CL_Component(rect, parent), impl(new PanelImpl())
{
  impl->parent = this;
  impl->slots.push_back(sig_paint().connect(impl.get(), &PanelImpl::draw));
}

void
PanelImpl::draw()
{
  CL_Display::push_translate(parent->get_screen_x(), parent->get_screen_y());
  CL_Rect rect = parent->get_position();
  Box::draw_panel(CL_Rect(CL_Point(0, 0), CL_Size(rect.get_width()-1, rect.get_height()-1)));
  CL_Display::pop_modelview();
}

/* EOF */
