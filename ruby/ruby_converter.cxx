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

#ifndef HEADER_RUBY_CONVERTER_HXX
#define HEADER_RUBY_CONVERTER_HXX

#include "flexlay_wrap.hxx"

// The following functions are defined in flexlay_wrap.i, a bit hacky but seems to work
VALUE ObjMapObject2Value(const ObjMapObject& arg);
VALUE ObjectBrush2Value(const ObjectBrush& arg);
VALUE CL_Point2Value(const CL_Point& arg);
VALUE CL_Pointf2Value(const CL_Pointf& arg);
VALUE CL_Color2Value(const CL_Color& arg);

template<> VALUE convert_to_ruby_value<float>(const float& arg)
{
  return rb_float_new(arg);
}

template<>
VALUE convert_to_ruby_value<ObjMapObject>(const ObjMapObject& arg)
{
  return ObjMapObject2Value(arg);
}

template<>
VALUE convert_to_ruby_value<ObjectBrush>(const ObjectBrush& arg)
{
  return ObjectBrush2Value(arg);
}

template<>
VALUE convert_to_ruby_value<CL_Color>(const CL_Color& arg)
{
  return CL_Color2Value(arg);
}

template<>
VALUE convert_to_ruby_value<CL_Point>(const CL_Point& arg)
{
  return CL_Point2Value(arg);
}

template<>
VALUE convert_to_ruby_value<CL_Pointf>(const CL_Pointf& arg)
{
  return CL_Pointf2Value(arg);
}

template<>
VALUE convert_to_ruby_value<int>(const int& arg)
{
  return INT2FIX(arg);
}

#endif

/* EOF */
