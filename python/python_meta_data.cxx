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

#include <iostream>
#include "python_meta_data.hxx"
#include "meta_data_impl.hxx"

typedef MetaDataGeneric<PythonObject> PythonMetaData;

MetaData make_metadata(PyObject* obj)
{
  return MetaData(SharedPtr<MetaDataImpl>(new PythonMetaData(PythonObject(obj))));
}

PyObject* get_python_object(const MetaData& data_obj)
{
  MetaDataImpl*    data = data_obj.get_impl().get();
  if (data)
    {
      PythonMetaData* pyobj = dynamic_cast<PythonMetaData*>(data);
      if (pyobj)
        {
          Py_XINCREF(pyobj->data.ptr());
          return pyobj->data.ptr();
        }
      else
        {
          std::cout << "Warning: MetaData isn't a python object" << std::endl;
          PyObject* p = Py_None;
          Py_XINCREF(p);
          return p;
        }
    }
  else
    {
      std::cout << "Warning: no MetaData associated with this object" << std::endl;
      PyObject* p = Py_None;
      Py_XINCREF(p);
      return p;
    }
}

/* EOF */
