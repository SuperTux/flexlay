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

#ifndef HEADER_COMMAND_HXX
#define HEADER_COMMAND_HXX

#include <iosfwd>
#include "shared_ptr.hxx"
#include "command_impl.hxx"

/** Command is an abstract base class for all data manipulating
    operations on EditorLayer or even EditorMap metedata. Each Command
    that manipulates data must provide a way to undo and redo the
    operations. */
class Command
{
private:
public:
  Command();
  Command(const SharedPtr<CommandImpl>& impl_);

  /** Execute the command */
  void execute();
  
  /** Execute the command a second time after the command got
      undo'ed */
  void redo();

  /** Undo the effects caused by execute() */
  void undo();

  std::string serialize();

private:
  SharedPtr<CommandImpl> impl;
};

#endif

/* EOF */
