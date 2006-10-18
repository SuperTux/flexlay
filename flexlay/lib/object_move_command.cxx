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

#include "object_layer.hxx"
#include "objmap_object.hxx"
#include "object_move_command.hxx"

class ObjectMoveCommandImpl : public CommandImpl
{
public:
  ObjectMoveCommandImpl() {}
  virtual ~ObjectMoveCommandImpl() {}

  ObjectLayer objmap;
  
  struct Obj {
    CL_Pointf old_pos;
    CL_Pointf new_pos;
    ObjMapObject obj;
  };
  
  typedef std::vector<Obj> Objects;
  Objects objects;

  void execute();
  void redo();
  void undo();

  std::string serialize();
};

ObjectMoveCommand::ObjectMoveCommand(const ObjectLayer& o)
  : impl(new ObjectMoveCommandImpl())
{
  impl->objmap = o;
}

ObjectMoveCommand::~ObjectMoveCommand()
{
}
  
void
ObjectMoveCommandImpl::execute()
{
}

void
ObjectMoveCommand::add_obj(const ObjMapObject& obj)
{
  ObjectMoveCommandImpl::Obj o;
  o.obj     = obj;
  o.old_pos = obj.get_pos();
  impl->objects.push_back(o);
}

void
ObjectMoveCommand::move_by(const CL_Pointf& delta)
{
  for(ObjectMoveCommandImpl::Objects::iterator i = impl->objects.begin(); 
      i != impl->objects.end();
      ++i)
    {
      i->new_pos = i->old_pos + delta;
      i->obj.set_pos(i->new_pos);
      i->obj.sig_move()(i->obj);
    }
}

void
ObjectMoveCommandImpl::redo()
{
  for(ObjectMoveCommandImpl::Objects::iterator i = objects.begin(); 
      i != objects.end();
      ++i)
    {
      i->obj.set_pos(i->new_pos);
    }  
}

void
ObjectMoveCommandImpl::undo()
{
  for(ObjectMoveCommandImpl::Objects::iterator i = objects.begin(); 
      i != objects.end();
      ++i)
    {
      i->obj.set_pos(i->old_pos);
    }
}

std::string
ObjectMoveCommandImpl::serialize()
{
  return "";
}

Command
ObjectMoveCommand::to_command()
{
  return Command(impl);
}

/* EOF */
