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

#include <iostream>
#include <ClanLib/Display/mouse.h>
#include "popup_menu.hxx"

PopupMenu::PopupMenu(const CL_Point& pos,  CL_Component* parent)
{
  menu = new CL_Menu(CL_Point(pos.x, pos.y), parent, NULL, true);
  
  menu->open();
  
  slots.push_back(CL_Mouse::sig_key_down().connect(this, &PopupMenu::on_mouse_up));
}

PopupMenu::~PopupMenu()
{
  //menu->release_mouse();
  delete menu;
}

void
PopupMenu::on_mouse_up(const CL_InputEvent& event)
{
  if (!menu->has_mouse_over() && !menu->has_mouse_in_submenus())
    {
      menu->collapse();
      // FIXME: *yuck*
      delete this;
    }
  else
    {
      // FIXME: this should be in the constructor, but doesn't work
      // there since the menu collapses then instantly
      menu->set_root_collapsing(true); 
    }
}

/* EOF */
