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

#ifndef HEADER_FLEXLAY_COMMAND_HPP
#define HEADER_FLEXLAY_COMMAND_HPP

#include <memory>
#include "command_impl.hpp"

/** Command is an abstract base class for all data manipulating
    operations on EditorLayer or even EditorMap metedata. Each Command
    that manipulates data must provide a way to undo and redo the
    operations. */
class Command
{
private:
public:
  Command();
  Command(const std::shared_ptr<CommandImpl>& impl_);

  /** Execute the command */
  void execute();

  /** Execute the command a second time after the command got
      undo'ed */
  void redo();

  /** Undo the effects caused by execute() */
  void undo();

  std::string serialize();

private:
  std::shared_ptr<CommandImpl> impl;
};

#endif

/* EOF */
