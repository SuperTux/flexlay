//  $Id: style_manager_windstille.cxx,v 1.1 2003/10/12 11:58:09 grumbel Exp $
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
#include "button_windstille.hxx"
#include "style_manager_windstille.hxx"

StyleManager_Windstille::StyleManager_Windstille(CL_ResourceManager *resources)
  : CL_StyleManager_Silver(resources)
{
}

void StyleManager_Windstille::connect_styles(const std::string &type, CL_Component *component)
{
  std::cout << "connect_style: " << type << std::endl;
  if (type == "button")
    {
      CL_Button *button = (CL_Button *)component;
      button->set_style(new Button_Windstille(button));
    }
  else
    {
      CL_StyleManager_Silver::connect_styles(type, component);
    }
}

/* EOF */
