//  $Id: editor.cxx,v 1.7 2003/10/11 08:11:59 grumbel Exp $
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

#include <ClanLib/Signals/signal_v0.h>
#include "../blitter.hxx"
#include "../python_functor.hxx"
#include "editor.hxx"

void connect(CL_Signal_v0& sig, PyObject* obj)
{
  sig = CL_Signal_v0();

  //std::cout << "Connecting functor: " << std::endl;
  new CL_Slot(sig.connect_functor(PythonFunctor(obj)));
}

void connect_v1(CL_Signal_v1<int>& sig, PyObject* obj)
{
  sig = CL_Signal_v1<int>();
  new CL_Slot(sig.connect_functor(PythonFunctor(obj)));
}

void connect_v2(CL_Signal_v2<int, int>& sig, PyObject* obj)
{
  sig = CL_Signal_v2<int, int>();
  new CL_Slot(sig.connect_functor(PythonFunctor(obj)));
}

/*
Tile
make_tile(const char* filename, 
          unsigned char red, unsigned char green, unsigned char blue, unsigned char alpha)
{
  return Tile(filename, CL_Color(red, green, blue, alpha));
}*/

/* EOF */
