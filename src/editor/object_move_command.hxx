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

#ifndef HEADER_OBJECT_COMMAND_HXX
#define HEADER_OBJECT_COMMAND_HXX

#include "command.hxx"

class EditorObjMap;

/** */
class ObjectMoveCommand : public Command
{
private:
  EditorObjMap* objmap;

  struct Obj {
    CL_Point old_pos;
    CL_Point new_pos;
    int id;
  };
  
  typedef std::vector<Obj> Objects;
  Objects objects;
public:
  ObjectMoveCommand(EditorObjMap* o);
  virtual ~ObjectMoveCommand() {}

  void add_obj(int id);

  void execute();
  void redo();
  void undo();
};

#endif

/* EOF */
