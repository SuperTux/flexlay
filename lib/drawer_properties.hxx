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

#ifndef HEADER_DRAWER_PROPERTIES_HXX
#define HEADER_DRAWER_PROPERTIES_HXX

#include <ClanLib/Display/color.h>
#include "brush.hxx"

class DrawerPropertiesImpl;

/** */
class DrawerProperties
{
private:
  static DrawerProperties* current_;
public:
  static DrawerProperties* current();

  DrawerProperties();
  ~DrawerProperties();

  /** Set the spacing that will be between the sprites that are drawn
      along the dabs */
  void  set_spacing(float spacing);
  float get_spacing() const;

  /** Set the base size of the Sprite, the real size itself can be
      affected by pressure and is than calculated by combining
      basesize and pressure or tilting */
  void  set_size(float s);
  float get_size() const;

  /** Set the base color, the real color itself is calculated from
      combining the base color with the current pressure or tilting */
  void     set_color(const CL_Color& color);
  CL_Color get_color() const;

  /** Set the brush to be used, its color and size settings are
      ignored */
  void  set_brush(const Brush& brush);
  Brush get_brush() const;

private:
  SharedPtr<DrawerPropertiesImpl> impl;
};

#endif

/* EOF */
