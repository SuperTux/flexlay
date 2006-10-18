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

#ifndef HEADER_PYTHON_FUNCTOR_HXX
#define HEADER_PYTHON_FUNCTOR_HXX

#include <ClanLib/Core/System/sharedptr.h>
#include "Python.h"

/** */
class PythonFunctor
{
private:
  PyObject* obj;
public:
  PythonFunctor();
  PythonFunctor(PyObject* o);
  PythonFunctor(const PythonFunctor& copy);
  PythonFunctor& operator=(const PythonFunctor& copy);

  ~PythonFunctor();

  void operator()();
  void operator()(int i);
  void operator()(int x, int y);
};

#endif

/* EOF */
