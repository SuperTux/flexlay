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

extern CL_ResourceManager* resources;
ObjectLayer ObjectLayer::current_;

class ObjectLayerImpl
{
public:
  ObjectLayer::Objs objects;
  int handle_count;
  CL_SlotContainer slots;
};

ObjectLayer::ObjectLayer()
  : impl(new ObjectLayerImpl())
{
  impl->handle_count = 0;
}

ObjectLayer::~ObjectLayer()
{
}

void
ObjectLayer::draw(EditorMapComponent* parent)
{
  for(Objs::iterator i = impl->objects.begin(); i != impl->objects.end(); ++i)
    {
      (*i)->draw();
    }
}

int
ObjectLayer::duplicate_object(int id)
{
  ObjMapObject* obj    = get_object(id);
  ObjMapObject* newobj = obj->duplicate(++impl->handle_count);
  impl->objects.push_back(newobj);  

  // FIXME: Move to scripting level
  newobj->set_pos(newobj->get_pos() + CL_Point(16, 16));

  return newobj->get_handle();
}

int
ObjectLayer::add_object(const CL_Sprite& sprite, const CL_Point& pos, const MetaData& data)
{
  ObjMapObject* obj = new ObjMapSpriteObject(++impl->handle_count, pos, data, sprite);

  impl->objects.push_back(obj);  

  return obj->get_handle();
}

ObjMapObject*
ObjectLayer::find_object(const CL_Point& click_pos)
{
  for(Objs::reverse_iterator i = impl->objects.rbegin(); i != impl->objects.rend(); ++i)
    {
      CL_Rect rect = (*i)->get_bound_rect();
     
      if (rect.is_inside(click_pos))
        return *i;
    }
  return 0;
}

void
ObjectLayer::delete_object(int id)
{
  for(Objs::iterator i = impl->objects.begin(); i != impl->objects.end(); ++i)
    {
      if ((*i)->get_handle() == id)
        {
          //delete *i;
          impl->objects.erase(i);
          break;
        }
    }
}

std::vector<ObjectLayer::Obj*>
ObjectLayer::get_selection(const CL_Rect& rect)
{
  std::vector<ObjectLayer::Obj*> selection;

  for(Objs::iterator i = impl->objects.begin(); i != impl->objects.end(); ++i)
    {
      // FIXME:
      if (rect.is_inside((*i)->get_pos()))
        {
          selection.push_back(*i);
        }
    }
  
  return selection;
}

ObjectLayer::Obj*
ObjectLayer::get_object(int id)
{
  for(Objs::iterator i = impl->objects.begin(); i != impl->objects.end(); ++i)
    if ((*i)->get_handle() == id)
      return *i;
  return 0;
}

ObjectLayer::Objs*
ObjectLayer::get_objects()
{
  return &impl->objects;
}

void
ObjectLayer::add_object(ObjMapObject* obj)
{
  impl->objects.push_back(obj);
}

int
ObjectLayer::get_next_object_handle()
{
  return ++impl->handle_count; 
}

Layer
ObjectLayer::to_layer()
{
  //return Layer(impl);
  return Layer();
}

/* EOF */
