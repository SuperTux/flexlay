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

#include <ClanLib/Display/display.h>
#include "editor_objmap.hxx"

extern CL_ResourceManager* resources;

EditorObjMap::EditorObjMap(EditorMap* p)
  : EditorMapLayer(p)
{
}

EditorObjMap::~EditorObjMap()
{
}

void
EditorObjMap::update(float delta)
{
  for(Objs::iterator i = objects.begin(); i != objects.end(); ++i)
    {
      (*i)->sprite.update(delta);
    }
}

void
EditorObjMap::draw()
{
  for(Objs::iterator i = objects.begin(); i != objects.end(); ++i)
    {
      (*i)->sprite.draw((*i)->pos.x, (*i)->pos.y);
    }
}

void
EditorObjMap::add_object(const char* name, const CL_Point& pos)
{
  Obj* obj = new Obj;
  obj->sprite = CL_Sprite(name, resources);
  obj->pos    = pos;
  objects.push_back(obj);  
}

EditorObjMap::Obj*
EditorObjMap::find_object(const CL_Point& pos)
{
  for(Objs::iterator i = objects.begin(); i != objects.end(); ++i)
    {
      if ((*i)->pos.x < pos.x
          && (*i)->pos.x + (*i)->sprite.get_width() >= pos.x
          && (*i)->pos.y < pos.y
          && (*i)->pos.y + (*i)->sprite.get_height() >= pos.y)
        {
          return *i;
        }
    }
  return 0;
}

/* EOF */
