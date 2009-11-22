//  Pingus - A free Lemmings clone
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

#include "sexpr_serializer.hpp"

SexprSerializer::SexprSerializer() 
{
}
  
void
SexprSerializer::register_group_start (const char* name)
{
  
}

void
SexprSerializer::register_group_end   ()
{
}

void
SexprSerializer::register_float (const char* name, float& value)
{
  
}

void
SexprSerializer::register_int   (const char* name, int& value)
{
}

void
SexprSerializer::register_string(const char* name, std::string& value)
{
}

/* EOF */
