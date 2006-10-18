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

#include "menu.hxx"
#include "menubar.hxx"

class MenubarItem
{
public:
  MenubarItem(const std::string& name_, Menu* menu_)
    : name(name_), menu(menu_){}

  std::string name;
  Menu* menu;
};

class MenubarImpl
{
public:
  typedef std::vector<MenubarItem> Items;
  Items items;
};

Menubar::Menubar(const CL_Point& pos, CL_Component* parent)
  : CL_Component(CL_Rect(pos, CL_Size(1, 1)), parent),
    impl(new MenubarImpl())
{
  
}

void
Menubar::add_submenu(const std::string& name, Menu* menu)
{
  impl->items.push_back(MenubarItem(name, menu));
}

/* EOF */
