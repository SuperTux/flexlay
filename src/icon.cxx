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
#include "icon.hxx"

class IconImpl
{
public:
  IconImpl(Icon* p) : parent(p) {}

  Icon* parent;

  std::vector<CL_Slot> slots;
  CL_Sprite sprite;
  std::string tooltip;
  bool draw_tooltip;
  bool down;
  CL_Signal_v0 sig_on_click;

  void draw();

  void mouse_up  (const CL_InputEvent& event);
  void mouse_down(const CL_InputEvent& event);
  void mouse_move(const CL_InputEvent& event);
};

Icon::Icon(const CL_Point& pos, const CL_Sprite& sprite, const std::string& tooltip, 
           CL_Component* parent)
  : CL_Component(CL_Rect(pos, CL_Size(34, 34)), parent),
    impl(new IconImpl(this))
{
  impl->sprite       = sprite;
  impl->tooltip      = tooltip;
  impl->draw_tooltip = true;
  impl->down         = false;

  impl->slots.push_back(sig_paint().connect(impl.get(), &IconImpl::draw));
  impl->slots.push_back(sig_mouse_down().connect(impl.get(), &IconImpl::mouse_down));
  impl->slots.push_back(sig_mouse_up().connect(impl.get(),   &IconImpl::mouse_up));
}

CL_Signal_v0&
Icon::sig_clicked()
{
  return impl->sig_on_click;
}
  
void
IconImpl::draw()
{
  CL_Color background(220, 220, 220);
  CL_Color background_hl(235, 235, 235);
  CL_Color background_sw(200, 200, 200);
  CL_Color highlight(255, 255, 255);
  CL_Color shadow(0, 0, 0);

  if (parent->has_mouse_over())
    if (!down)
      CL_Display::fill_rect(CL_Rect(CL_Point(0, 0), CL_Size(34, 34)), background_hl);
    else
      CL_Display::fill_rect(CL_Rect(CL_Point(0, 0), CL_Size(34, 34)), background_sw);
  else
    CL_Display::fill_rect(CL_Rect(CL_Point(0, 0), CL_Size(34, 34)), background);

  if (parent->has_mouse_over() && down)
    std::swap(highlight, shadow);

  if (parent->has_mouse_over())
    { 
      CL_Display::draw_line(0, 0,  0, 33, highlight);
      CL_Display::draw_line(0, 0, 33,  0, highlight);
      
      CL_Display::draw_line(33,  0, 33, 33, shadow);
      CL_Display::draw_line( 0, 33, 33, 33, shadow);
    }

  sprite.set_alignment(origin_center);
  sprite.draw(17, 17);
}

void
IconImpl::mouse_up  (const CL_InputEvent& event)
{
  down         = false;
  parent->release_mouse();  

  if (parent->has_mouse_over())
    {
      sig_on_click();
    }
}

void
IconImpl::mouse_down(const CL_InputEvent& event)
{
  down         = true;
  parent->capture_mouse();
}

void 
IconImpl::mouse_move(const CL_InputEvent& event)
{
  
}

/* EOF */
