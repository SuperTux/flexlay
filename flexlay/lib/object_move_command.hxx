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

#ifndef HEADER_OBJECT_COMMAND_HXX
#define HEADER_OBJECT_COMMAND_HXX

#include "command.hxx"

class ObjMapObject;
class ObjectLayer;

class ObjectMoveCommandImpl;

/** */
class ObjectMoveCommand
{
public:
  ObjectMoveCommand(const ObjectLayer& o);
  virtual ~ObjectMoveCommand();

  void add_obj(const ObjMapObject& obj);
  void move_by(const CL_Pointf& delta);

  Command to_command();
private:
  SharedPtr<ObjectMoveCommandImpl> impl;
};

#endif

/* EOF */
