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

#include "command_group.hpp"

class CommandGroupImpl : public CommandImpl
{
public:
};

CommandGroup::CommandGroup()
{

}

CommandGroup::~CommandGroup()
{
}

void
CommandGroup::add_command(Command* c)
{
  commands.push_back(c);
}

void
CommandGroup::execute()
{
  for(Commands::iterator i = commands.begin(); i != commands.end(); ++i)
    (*i)->execute();
}

void
CommandGroup::undo()
{
  for(Commands::iterator i = commands.begin(); i != commands.end(); ++i)
    (*i)->undo();
}

void
CommandGroup::redo()
{
  for(Commands::iterator i = commands.begin(); i != commands.end(); ++i)
    (*i)->redo();
}

/* EOF */
