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
#include "layer_impl.hxx"
#include "layer.hxx"

Layer::Layer()
{
}

Layer::Layer(SharedPtr<LayerImpl> i)
  : impl(i)
{
}

Layer::~Layer()
{
}

void
Layer::draw(EditorMapComponent* parent) 
{ 
  if (impl.get())
    impl->draw(parent);    
}
  
bool
Layer::has_bounding_rect() const 
{
  if (impl.get())
    return impl->has_bounding_rect(); 
  else
    return false;
} 

CL_Rect
Layer::get_bounding_rect() 
{ 
  if (impl.get())
    return impl->get_bounding_rect();
  else
    return CL_Rect();
}

MetaData
Layer::get_metadata() const
{
  if (impl.get())
    return impl->data; 
  else
    return MetaData();
}

void
Layer::set_metadata(MetaData data_)
{
  if (impl.get())
    impl->data = data_;
}

/* EOF */
