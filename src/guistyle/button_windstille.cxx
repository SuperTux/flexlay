//  $Id: button_windstille.cxx,v 1.1 2003/10/12 11:58:09 grumbel Exp $
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
#include "../fonts.hxx"
#include "button_windstille.hxx"

void
Button_Windstille::on_paint()
{
  int button_width  = button->get_width();
  int button_height = button->get_height();
  
  if(button->is_down())
    {
      CL_Display::fill_rect(CL_Rect(CL_Point(0, 0), CL_Size(button_width, button_height)), 
                            CL_Gradient(CL_Color(0,0,0,228),
                                        CL_Color(0,0,0,228),
                                        CL_Color(0,0,0,128),
                                        CL_Color(0,0,0,128)));

      CL_Display::draw_rect(CL_Rect(CL_Point(0, 0), CL_Size(button_width, button_height)), 
                            CL_Color(255,255,255, 80));
      Fonts::copyright.set_alignment(origin_center);
      Fonts::copyright.draw(button_width/2, button_height/2, button->get_text());
    }
  else
    {
      if (button->has_mouse_over())
        CL_Display::fill_rect(CL_Rect(CL_Point(0, 0), CL_Size(button_width, button_height)), 
                              CL_Gradient(CL_Color(150, 150,250,228),
                                          CL_Color(150, 150,250,228),
                                          CL_Color(150, 150,250,128),
                                          CL_Color(150, 150,250,128)));
      else
        CL_Display::fill_rect(CL_Rect(CL_Point(0, 0), CL_Size(button_width, button_height)), 
                              CL_Gradient(CL_Color(0,0,100,228),
                                          CL_Color(0,0,100,228),
                                          CL_Color(0,0,100,128),
                                          CL_Color(0,0,100,128)));

      CL_Display::draw_rect(CL_Rect(CL_Point(0, 0), CL_Size(button_width, button_height)), 
                            CL_Color(255,255,255, 180));
      Fonts::copyright.set_alignment(origin_center);
      Fonts::copyright.draw(button_width/2, button_height/2, button->get_text());
    }
}
 
Button_Windstille::Button_Windstille(CL_Button *button)
  : CL_ComponentStyle(button), 
    button(button)
{
  slot_paint = button->sig_paint().connect(this, &Button_Windstille::on_paint);
}

/* EOF */
