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
#include <ClanLib/Display/display.h>
#include <ClanLib/Core/Math/origin.h>
#include <ClanLib/Core/System/error.h>
#include "objmap_object.hxx"
#include "objmap_sprite_object.hxx"
#include "object_layer.hxx"
#include "layer_impl.hxx"

extern CL_ResourceManager* resources;
ObjectLayer ObjectLayer::current_;

class ObjectLayerImpl : public LayerImpl
{
public:
  ObjectLayer::Objs objects;
  CL_SlotContainer slots;

  ObjectLayerImpl() {}
  virtual ~ObjectLayerImpl() {}
  
  void draw(EditorMapComponent* parent);
  bool has_bounding_rect() const { return false; }
};

ObjectLayer::ObjectLayer()
  : impl(new ObjectLayerImpl())
{
}

ObjectLayer::~ObjectLayer()
{
}

void
ObjectLayerImpl::draw(EditorMapComponent* parent)
{
  for(ObjectLayer::Objs::iterator i = objects.begin(); i != objects.end(); ++i)
    {
      (*i).draw();
    }
}

ObjMapObject
ObjectLayer::find_object(const CL_Point& click_pos)
{
  for(Objs::reverse_iterator i = impl->objects.rbegin(); i != impl->objects.rend(); ++i)
    {
      CL_Rect rect = (*i).get_bound_rect();
     
      if (rect.is_inside(click_pos))
        return *i;
    }
  return ObjMapObject();
}

void
ObjectLayer::delete_object(const ObjMapObject& obj)
{
  for(Objs::iterator i = impl->objects.begin(); i != impl->objects.end(); ++i)
    {
      if (obj == (*i))
        {
          impl->objects.erase(i);
          break;
        }
    }
}

ObjectLayer::Objs
ObjectLayer::get_selection(const CL_Rect& rect)
{
  Objs selection;

  for(Objs::iterator i = impl->objects.begin(); i != impl->objects.end(); ++i)
    {
      // FIXME:
      if (rect.is_inside((*i).get_pos()))
        {
          selection.push_back(*i);
        }
    }
  
  return selection;
}

ObjectLayer::Objs
ObjectLayer::get_objects()
{
  return impl->objects;
}

void
ObjectLayer::add_object(const ObjMapObject& obj)
{
  impl->objects.push_back(obj);
}

Layer
ObjectLayer::to_layer()
{
  return Layer(impl);
}

/* EOF */
