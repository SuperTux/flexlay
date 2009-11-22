//  Flexlay - A Generic 2D Game Editor
//  Copyright (C) 2002 Ingo Ruhnke <grumbel@gmx.de>
//
//  This program is free software: you can redistribute it and/or modify
//  it under the terms of the GNU General Public License as published by
//  the Free Software Foundation, either version 3 of the License, or
//  (at your option) any later version.
//  
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//  GNU General Public License for more details.
//  
//  You should have received a copy of the GNU General Public License
//  along with this program.  If not, see <http://www.gnu.org/licenses/>.

#include "objmap_object_impl.hpp"

ObjMapObject::ObjMapObject()
{
}

ObjMapObject::ObjMapObject(const boost::shared_ptr<ObjMapObjectImpl>& impl_)
  : impl(impl_)
{
}

CL_Pointf
ObjMapObject::get_pos() const 
{
  if (impl.get())
    return impl->pos; 
  else
    return CL_Pointf();
}

void
ObjMapObject::set_pos(const CL_Pointf& p) 
{
  if (impl.get())
  {
    impl->pos = p; 
  }
}

MetaData
ObjMapObject::get_metadata() const
{
  if (impl.get())
    return impl->data; 
  else
    return MetaData();
}

void
ObjMapObject::set_metadata(const MetaData& data_)
{
  if (impl.get())
    impl->data = data_;
}

void
ObjMapObject::draw(CL_GraphicContext* gc)
{
  if (impl.get())
    impl->draw(gc);
}

CL_Rectf
ObjMapObject::get_bound_rect() const
{
  if (impl.get())
    return impl->get_bound_rect();
  else
    return CL_Rect();
}

bool
ObjMapObject::is_null() const
{
  return !impl.get();
}

bool
ObjMapObject::operator==(const ObjMapObject& obj) const
{
  return impl.get() == obj.impl.get();
}

bool
ObjMapObject::operator<(const ObjMapObject& obj) const
{
  return impl.get() < obj.impl.get();
}

CL_Signal_v1<ObjMapObject>&
ObjMapObject::sig_select()
{
  return impl->on_select;
}

CL_Signal_v1<ObjMapObject>&
ObjMapObject::sig_deselect()
{
  return impl->on_deselect;
}

CL_Signal_v1<ObjMapObject>&
ObjMapObject::sig_move()
{
  return impl->on_move;
}

void
ObjMapObject::add_control_points()
{
  impl->add_control_points();
}

void
ObjMapObject::update_control_points()
{
  impl->update_control_points();
}

/* EOF */
