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

#ifndef HEADER_FLEXLAY_WRAP_HXX
#define HEADER_FLEXLAY_WRAP_HXX

#include <ClanLib/Display/color.h>
#include <iostream>
#include <typeinfo>
#include "ruby.h"

class CL_Point;
class CL_Pointf;
class ObjectBrush;
class ObjMapObject;

template<class C>
VALUE convert_to_ruby_value(const C& c)
{
  std::cout << "Error: conversion for type '" << typeid(c).name() << "' missing" << std::endl;
  return Qnil;
}

template<> VALUE convert_to_ruby_value<ObjMapObject>(const ObjMapObject& arg);
template<> VALUE convert_to_ruby_value<ObjectBrush>(const ObjectBrush& arg);
template<> VALUE convert_to_ruby_value<CL_Color>(const CL_Color& arg);
template<> VALUE convert_to_ruby_value<int>(const int& arg);
template<> VALUE convert_to_ruby_value<float>(const float& arg);

#endif

/* EOF */
