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
#include <ClanLib/Core/core_iostream.h>
#include <ClanLib/Display/display.h>
#include <ClanLib/Display/sprite.h>
#include "object_layer.hxx"
#include "objmap_control_point.hxx"
#include "objmap_rect_object.hxx"
#include "flexlay.hxx"

class ObjMapRectObjectImpl : public ObjMapObjectImpl
{
public:
  CL_Size  size;
  CL_Color color;

  void draw();
  CL_Rect get_bound_rect() const;
  void add_control_points();
};

ObjMapRectObject::ObjMapRectObject(const CL_Rect&  rect_,
                                   const CL_Color& color_,
                                   const MetaData& data_)  
  : impl(new ObjMapRectObjectImpl)
{
  impl->pos   = CL_Point(rect_.left, rect_.top);
  impl->size  = CL_Size(rect_.get_width(), rect_.get_height());
  impl->color = color_;
  impl->data  = data_;
}

void
ObjMapRectObjectImpl::draw()
{
  CL_Display::fill_rect(get_bound_rect(), color);
}

CL_Rect
ObjMapRectObjectImpl::get_bound_rect() const
{
  return CL_Rect(pos, size);
}

ObjMapObject
ObjMapRectObject::to_object()
{
  return ObjMapObject(SharedPtr<ObjMapObjectImpl>(impl));
}

void
ObjMapRectObjectImpl::add_control_points()
{
  std::cout << "Adding control poinst..." << std::endl;
  ObjectLayer objmap = ObjectLayer::current();

  objmap.add_control_point(ObjMapControlPoint(CL_Sprite("resize_vert", &(Flexlay::current()->resources)), 
                                              CL_Point(0, 0), MetaData()));
}

/* EOF */
