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

#include <iostream>
#include "popup_menu.hxx"

PopupMenu::PopupMenu(const CL_Point& pos,  CL_Component* parent)
{
  menu = new CL_Menu(CL_Point(pos.x, pos.y), parent, NULL, true);
  
  slots.push_back(menu->sig_mouse_up().connect(this, &PopupMenu::on_mouse_up));

  menu->create_item("Properties");
  menu->create_item("Delete");
  menu->create_item("Foobar");
  menu->create_item("Foobarbla/aeu");
  menu->create_item("Foobarbla/aeuaeaeu");
  menu->create_item("Something");

  //menu->reposition();
  menu->open();
  menu->capture_mouse();
  std::cout << "Menu open" << std::endl;
}

PopupMenu::~PopupMenu()
{
  menu->release_mouse();
}

void
PopupMenu::on_mouse_up(const CL_InputEvent& event)
{
  if (!menu->has_mouse_over())
    {
      std::cout << "Menu close" << std::endl;
      menu->collapse();
      // FIXME: *yuck*
      delete this;
    }
  else
    {
      
    }
}

/* EOF */
