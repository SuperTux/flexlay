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
#include "command_impl.hxx"
#include "object_delete_command.hxx"

class ObjectDeleteCommandImpl : public CommandImpl
{
public:
  typedef std::vector<ObjMapObject> Objects;

  ObjectLayer object_layer;
  Objects objects;
  
  ObjectDeleteCommandImpl() {}
  virtual ~ObjectDeleteCommandImpl() {}

  void execute();
  void redo();
  void undo();
  std::string serialize() { return ""; }
};

ObjectDeleteCommand::ObjectDeleteCommand(const ObjectLayer& o)
  : impl(new ObjectDeleteCommandImpl())
{
  impl->object_layer = o;
}

void
ObjectDeleteCommand::add_object(const ObjMapObject& obj)
{
  impl->objects.push_back(obj);
}

void
ObjectDeleteCommandImpl::execute()
{
  for(Objects::iterator i = objects.begin(); i != objects.end(); ++i)
    {
      object_layer.delete_object(*i);
    }
}

void
ObjectDeleteCommandImpl::redo()
{
  execute();
}

void
ObjectDeleteCommandImpl::undo()
{
  for(Objects::iterator i = objects.begin(); i != objects.end(); ++i)
    {
      object_layer.add_object(*i);
    }
}

Command
ObjectDeleteCommand::to_command()
{
  return Command(impl);
}

/* EOF */
