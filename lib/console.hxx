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

#ifndef HEADER_CONSOLE_HXX
#define HEADER_CONSOLE_HXX

#include <ClanLib/GUI/component.h>
#include "shared_ptr.hxx"

class CL_Font;
class CL_Size;
class ConsoleImpl;

/** */
class Console : public CL_Component
{
protected:
  virtual ~Console();
public:
  Console(/*const CL_Font& font, */const CL_Rect& rect, CL_Component* parent);

  /** Write something to the console */
  void write(const std::string& );
  void clearscr();
private:
  SharedPtr<ConsoleImpl> impl;
};

#endif

/* EOF */
