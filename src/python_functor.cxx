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

#include "Python.h"
#include "python_functor.hxx"

PythonFunctor::PythonFunctor()
  : obj(0)
{
}

PythonFunctor::PythonFunctor(PyObject* o)
{
  obj = o;
  Py_XINCREF(obj);
}

PythonFunctor::PythonFunctor(const PythonFunctor& copy)
{
  obj = copy.obj;
  Py_XINCREF(obj);
}

PythonFunctor::~PythonFunctor()
{
  Py_XDECREF(obj);
}

PythonFunctor&
PythonFunctor::operator=(const PythonFunctor& copy)
{
  if (this != &copy)
    {
      obj = copy.obj;
      Py_XINCREF(obj);
    }
  return *this;  
}

void
PythonFunctor::operator()()
{
  if (obj)
    {
      PyObject* arglist = PyTuple_New(0);
      if (PyEval_CallObject(obj,  arglist) == 0)
        {
          if (PyErr_Occurred())
            {
              PyErr_Print();
            }
        }
      Py_DECREF(arglist);
    }
}

void
PythonFunctor::operator()(int i)
{
  if (obj)
    {
      PyObject* arglist = Py_BuildValue("(i)", i);
      if (PyEval_CallObject(obj,  arglist) == 0)
        {
          if (PyErr_Occurred())
            {
              PyErr_Print();
            }
        }
      //Py_DECREF(arglist);
    }  
}

void
PythonFunctor::operator()(int x, int y)
{
  if (obj)
    {
      PyObject* arglist = Py_BuildValue("(i, i)", x, y);
      if (PyEval_CallObject(obj,  arglist) == 0)
        {
          if (PyErr_Occurred())
            {
              PyErr_Print();
            }
        }
      //Py_DECREF(arglist);
    }  
}

/* EOF */
