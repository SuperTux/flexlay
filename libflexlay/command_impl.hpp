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

#ifndef HEADER_FLEXLAY_COMMAND_IMPL_HPP
#define HEADER_FLEXLAY_COMMAND_IMPL_HPP

#include <string>

class CommandImpl
{
private:
public:
  virtual ~CommandImpl() {}

  /** Execute the command */
  virtual void execute() =0;

  /** Execute the command a second time after the command got
      undo'ed */
  virtual void redo() =0;

  /** Undo the effects caused by execute() */
  virtual void undo() =0;

  virtual std::string serialize() =0;
};

#endif

/* EOF */
