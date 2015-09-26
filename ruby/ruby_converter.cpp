//  Pingus - A free Lemmings clone
//  Copyright (C) 2002 Ingo Ruhnke <grumbel@gmail.com>
//
//  This program is free software: you can redistribute it and/or modify
//  it under the terms of the GNU General Public License as published by
//  the Free Software Foundation, either version 3 of the License, or
//  (at your option) any later version.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//  GNU General Public License for more details.
//
//  You should have received a copy of the GNU General Public License
//  along with this program.  If not, see <http://www.gnu.org/licenses/>.

#ifndef HEADER_RUBY_CONVERTER_HXX
#define HEADER_RUBY_CONVERTER_HXX

#include "flexlay_wrap.hpp"

// The following functions are defined in flexlay_wrap.i, a bit hacky but seems to work
VALUE ObjMapObject2Value(const ObjMapObject& arg);
VALUE ObjectBrush2Value(const ObjectBrush& arg);
VALUE Point2Value(const Point& arg);
VALUE Pointf2Value(const Pointf& arg);
VALUE Color2Value(const Color& arg);

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
VALUE convert_to_ruby_value<Color>(const Color& arg)
{
  return Color2Value(arg);
}

template<>
VALUE convert_to_ruby_value<Point>(const Point& arg)
{
  return Point2Value(arg);
}

template<>
VALUE convert_to_ruby_value<Pointf>(const Pointf& arg)
{
  return Pointf2Value(arg);
}

template<>
VALUE convert_to_ruby_value<int>(const int& arg)
{
  return INT2FIX(arg);
}

template<>
VALUE convert_to_ruby_value<std::string>(const std::string& arg)
{
  return rb_str_new(arg.c_str(), arg.size());
}

#endif

/* EOF */
