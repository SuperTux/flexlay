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
  ObjMapObject* obj;

  ObjectAddCommandImpl() {}
  virtual ~ObjectAddCommandImpl() {}

  void execute();
  void undo();
  void redo();

  std::string serialize();
};

ObjectAddCommand::ObjectAddCommand(const ObjectLayer& objmap_, ObjMapObject* obj_)
  : impl(new ObjectAddCommandImpl())
{
  impl->objmap = objmap_;
  impl->obj    = obj_;
}

ObjectAddCommand::~ObjectAddCommand()
{
}

int
ObjectAddCommand::get_handle() const
{ 
  return impl->obj->get_handle(); 
}

void
ObjectAddCommandImpl::execute()
{
  objmap.add_object(obj);
}

void
ObjectAddCommandImpl::undo()
{
  objmap.delete_object(obj->get_handle());
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
