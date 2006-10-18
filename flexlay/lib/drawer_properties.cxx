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

#include "drawer_properties.hxx"

DrawerProperties* DrawerProperties::current_ = 0;

class DrawerPropertiesImpl
{
public:
  CL_Color  color;
  float     base_size;
  float     spacing;
  Brush     brush;
};

DrawerProperties*
DrawerProperties::current()
{
  if (!current_)
    return (current_ = new DrawerProperties());
  else
    return current_;
}

DrawerProperties::DrawerProperties()
  : impl(new DrawerPropertiesImpl())
{
  impl->color     = CL_Color(255, 255, 255, 255);
  impl->base_size = 1.0f;
    
  impl->base_size = 1.0f;
  impl->spacing   = 15.0f;
}

void
DrawerProperties::set_spacing(float spacing_)
{
  impl->spacing = spacing_;
}

float
DrawerProperties::get_spacing() const
{
  return impl->spacing;
}

void
DrawerProperties::set_size(float s)
{
  impl->base_size = s;
}

float
DrawerProperties::get_size() const
{
  return impl->base_size;
}

void
DrawerProperties::set_color(const CL_Color& color_)
{
  impl->color = color_;
}

CL_Color
DrawerProperties::get_color() const
{
  return impl->color;
}

void
DrawerProperties::set_brush(const Brush& brush)
{
  impl->brush = brush;
}

Brush
DrawerProperties::get_brush() const
{
  return impl->brush;
}

/* EOF */
