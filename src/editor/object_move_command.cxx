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

#include "editor_objmap.hxx"
#include "objmap_object.hxx"
#include "object_move_command.hxx"

ObjectMoveCommand::ObjectMoveCommand(EditorObjMap* o)
  : objmap(o)
{
  
}
  
void
ObjectMoveCommand::execute()
{
  for(Objects::iterator i = objects.begin(); 
      i != objects.end();
      ++i)
    {
      ObjMapObject* obj = objmap->get_object(i->id);
      if (obj) 
        {
          i->new_pos = obj->get_pos();
        }
    }
}

void
ObjectMoveCommand::add_obj(int id)
{
  ObjMapObject* obj = objmap->get_object(id);

  if (obj)
    {
      Obj o;
      o.id      = id;
      o.old_pos = obj->get_pos();
      objects.push_back(o);
    }
}

void
ObjectMoveCommand::redo()
{
  for(Objects::iterator i = objects.begin(); 
      i != objects.end();
      ++i)
    {
      ObjMapObject* obj = objmap->get_object(i->id);
      if (obj)
        {
          obj->set_pos(i->new_pos);
        }
    }  
}

void
ObjectMoveCommand::undo()
{
  for(Objects::iterator i = objects.begin(); 
      i != objects.end();
      ++i)
    {
      ObjMapObject* obj = objmap->get_object(i->id);
      if (obj)
        {
          obj->set_pos(i->old_pos);
        }
    }
}

/* EOF */
