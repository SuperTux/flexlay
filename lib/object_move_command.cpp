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

#include "object_layer.hpp"
#include "object_move_command.hpp"

class ObjectMoveCommandImpl : public CommandImpl
{
public:
  ObjectMoveCommandImpl() {}
  virtual ~ObjectMoveCommandImpl() {}

  ObjectLayer objmap;

  struct Obj {
    Pointf old_pos;
    Pointf new_pos;
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
ObjectMoveCommand::move_by(const Pointf& delta)
{
  for(auto it = impl->objects.begin(); it != impl->objects.end(); ++it)
  {
    it->new_pos = it->old_pos + delta;
    it->obj.set_pos(it->new_pos);
    it->obj.sig_move()(it->obj);
  }
}

void
ObjectMoveCommandImpl::redo()
{
  for(auto it = objects.begin(); it != objects.end(); ++it)
  {
    it->obj.set_pos(it->new_pos);
  }
}

void
ObjectMoveCommandImpl::undo()
{
  for(auto it = objects.begin(); it != objects.end(); ++it)
  {
    it->obj.set_pos(it->old_pos);
  }
}

std::string
ObjectMoveCommandImpl::serialize()
{
  return std::string();
}

Command
ObjectMoveCommand::to_command()
{
  return Command(impl);
}

/* EOF */
