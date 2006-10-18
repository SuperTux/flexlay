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
#include "object_add_command.hxx"

class ObjectAddCommandImpl : public CommandImpl
{
public:
  ObjectLayer objmap;
  std::vector<ObjMapObject> objs;

  ObjectAddCommandImpl() {}
  virtual ~ObjectAddCommandImpl() {}

  void execute();
  void undo();
  void redo();

  std::string serialize();
};

ObjectAddCommand::ObjectAddCommand(const ObjectLayer& objmap_)
  : impl(new ObjectAddCommandImpl())
{
  impl->objmap = objmap_;
}

ObjectAddCommand::~ObjectAddCommand()
{
}

/*int
ObjectAddCommand::get_handle() const
{ 
  return impl->obj->get_handle(); 
}*/

void
ObjectAddCommand::add_object(const ObjMapObject& obj)
{
  impl->objs.push_back(obj);
}

void
ObjectAddCommandImpl::execute()
{
  for(std::vector<ObjMapObject>::iterator i = objs.begin(); i != objs.end(); ++i)
    objmap.add_object(*i);
}

void
ObjectAddCommandImpl::undo()
{
  for(std::vector<ObjMapObject>::iterator i = objs.begin(); i != objs.end(); ++i)
    objmap.delete_object(*i);
}

void
ObjectAddCommandImpl::redo()
{
  execute();
}

std::string
ObjectAddCommandImpl::serialize()
{
  return "";
}

Command
ObjectAddCommand::to_command()
{
  return Command(impl);
}

/* EOF */
