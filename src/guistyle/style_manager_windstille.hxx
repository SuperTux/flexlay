//  $Id: style_manager_windstille.hxx,v 1.1 2003/10/12 11:58:09 grumbel Exp $
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

#ifndef HEADER_STYLE_MANAGER_WINDSTILLE_HXX
#define HEADER_STYLE_MANAGER_WINDSTILLE_HXX

#include <ClanLib/gui.h>
#include <ClanLib/guistylesilver.h>

/** */
class StyleManager_Windstille : public CL_StyleManager_Silver
{
private:
public:
  StyleManager_Windstille(CL_ResourceManager *resources);
  
  // Connect component styles to component.
  // The 'type' parameter indicates what type the component is.
  virtual void connect_styles(const std::string &type, CL_Component *owner);
};

#endif

/* EOF */
