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

#include "sexpr_parser.hxx"

PyObject*
build_py_sexpr(lisp_object_t* cur)
{
  if (lisp_cons_p(cur))
    {
      PyObject* lst = PyList_New(0);
  
      while (cur)
        {
          PyList_Append(lst, build_py_sexpr(lisp_car(cur)));
      
          cur = lisp_cdr(cur);
        }
      
      return lst;
    }
  else if (lisp_string_p(cur))
    {
      return PyString_FromString(lisp_string(cur));
    }
  else if (lisp_symbol_p(cur))
    {
      return PyString_FromString(lisp_symbol(cur));
    }
  else if (lisp_integer_p(cur))
    {
      return PyInt_FromLong(lisp_integer(cur));
    }
  else if (lisp_real_p(cur))
    {
      return PyFloat_FromDouble(lisp_real(cur));
    }
  else if (lisp_boolean_p(cur))
    {
      if (lisp_boolean(cur))
        {
          Py_INCREF(Py_True);
          return Py_True;
        }
      else
        {
          Py_INCREF(Py_False);
          return Py_False;
        }
    }
  else
    {
      return Py_None;
    }
}

PyObject*
sexpr_read_from_file(const char* filename)
{
  lisp_object_t* cur = lisp_read_from_file(filename);

  if (cur)
    {
      PyObject* obj = build_py_sexpr(cur);
      lisp_free(cur);
      return obj;
    }
  else
    {
      return Py_None;
    }
}

/* EOF */
