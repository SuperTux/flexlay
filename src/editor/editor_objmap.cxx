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

#include <iostream>
#include <ClanLib/Display/display.h>
#include <ClanLib/Core/Math/origin.h>
#include "objmap_object.hxx"
#include "objmap_sprite_object.hxx"
#include "editor_objmap.hxx"

extern CL_ResourceManager* resources;

EditorObjMap::EditorObjMap()
{
  handle_count = 0;
}

EditorObjMap::~EditorObjMap()
{
}

void
EditorObjMap::draw(EditorMapComponent* parent)
{
  for(Objs::iterator i = objects.begin(); i != objects.end(); ++i)
    {
      (*i)->draw();
    }
}

int
EditorObjMap::add_object(const CL_Sprite& sprite, const CL_Point& pos, const SCMObj& data)
{
  ObjMapObject* obj = new ObjMapSpriteObject(++handle_count, pos, data, sprite);

  objects.push_back(obj);  

  return obj->get_handle();
}

CL_Rect
EditorObjMap::get_bounding_rect(const CL_Sprite& sprite)
{
  // FIXME: TEST ME
  CL_Point  align = CL_Point(0, 0);
  CL_Origin origin_e;
      
  sprite.get_alignment(origin_e, align.x, align.y);

  CL_Point origin = calc_origin(origin_e, CL_Size(sprite.get_width(),
                                                  sprite.get_height()));
  align.x = -align.x;
      
  return CL_Rect(origin + align, 
                 CL_Size(sprite.get_width(), 
                         sprite.get_height()));
}

ObjMapObject*
EditorObjMap::find_object(const CL_Point& click_pos)
{
  for(Objs::iterator i = objects.begin(); i != objects.end(); ++i)
    {
      CL_Rect rect = (*i)->get_bound_rect();
     
      if (rect.is_inside(click_pos))
        return *i;
    }
  return 0;
}

std::vector<EditorObjMap::Obj*>
EditorObjMap::get_selection(const CL_Rect& rect)
{
  std::vector<EditorObjMap::Obj*> selection;

  for(Objs::iterator i = objects.begin(); i != objects.end(); ++i)
    {
      // FIXME:
      if (rect.is_inside((*i)->get_pos()))
        {
          selection.push_back(*i);
        }
    }
  
  return selection;
}

EditorObjMap::Obj*
EditorObjMap::get_object(int id)
{
  for(Objs::iterator i = objects.begin(); i != objects.end(); ++i)
    if ((*i)->get_handle() == id)
      return *i;
  return 0;
}

EditorObjMap::Objs*
EditorObjMap::get_objects()
{
  return &objects;
}

/* EOF */
