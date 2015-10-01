// Flexlay - A Generic 2D Game Editor
// Copyright (C) 2002 Ingo Ruhnke <grumbel@gmail.com>
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <http://www.gnu.org/licenses/>.

#ifndef HEADER_FLEXLAY_OBJECT_ADD_COMMAND_HPP
#define HEADER_FLEXLAY_OBJECT_ADD_COMMAND_HPP

#include "command.hpp"

class ObjMapObject;
class ObjectAddCommandImpl;
class ObjectLayer;

/** ObjectAddCommand adds on object to an ObjectLayer, the user needs
    to supply an Object together with the \a ObjectLayer to which it
    should be added. FIXME: position should be part of the command,
    not the object */
class ObjectAddCommand
{
public:
  ObjectAddCommand(const ObjectLayer& layer);
  virtual ~ObjectAddCommand();

  void add_object(const ObjMapObject& obj);

  //int get_handle() const;

  Command to_command();

private:
  std::shared_ptr<ObjectAddCommandImpl> impl;
};

#endif

/* EOF */
