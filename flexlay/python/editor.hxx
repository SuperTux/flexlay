//  $Id: editor.hxx,v 1.8 2003/10/11 08:11:59 grumbel Exp $
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

#ifndef HEADER_SCRIPTING_EDITOR_HXX
#define HEADER_SCRIPTING_EDITOR_HXX

#include <string>

#include <ClanLib/Display/sprite.h>
#include <ClanLib/Display/display.h>
#include <ClanLib/Signals/signal_v0.h>
#include "../tile.hxx"

#ifdef SWIGPYTHON
#include "Python.h"

void connect(CL_Signal_v0& sig, PyObject* obj);
void connect_v1(CL_Signal_v1<int>& sig, PyObject* obj);
void connect_v2(CL_Signal_v2<int, int>& sig, PyObject* obj);
#endif

/*Tile make_tile(const char* filename, 
  unsigned char red, unsigned char green, unsigned char blue, unsigned char alpha);*/

#endif

/* EOF */
