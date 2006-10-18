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

#ifndef HEADER_SEXPR_SERIALIZER_HXX
#define HEADER_SEXPR_SERIALIZER_HXX

/** */
class SexprSerializer
{
private:
  
public:
  SexprSerializer() {}
  
  voi  register_group_start (const char* name);
  voi  register_group_end   ();

  void register_float (const char* name, float& value);
  void register_int   (const char* name, int& value);
  void register_string(const char* name, std::string& value);

  /** Return a string representing the serialized objects */
  std::string get_string();
};

#endif

/* EOF */
