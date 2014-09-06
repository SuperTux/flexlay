// Flexlay - A Generic 2D Game Editor
// Copyright (C) 2002 Ingo Ruhnke <grumbel@gmail.com>
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <http://www.gnu.org/licenses/>.

#ifndef HEADER_PROPERTY_HPP
#define HEADER_PROPERTY_HPP

#include <string>

class PropertyValue
{
public:
  enum Type {
    TYPE_BOOL,
    TYPE_INT,
    TYPE_FLOAT,
    TYPE_STRING
  };

private:
  Type m_type;

  bool m_bool;
  int m_int;
  float m_float;
  std::string m_string;

public:
  PropertyValue() :
    m_type(TYPE_BOOL),
    m_bool(false)
  {}

  PropertyValue(bool v) :
    m_type(TYPE_BOOL),
    m_bool(v)
  {}

  PropertyValue(int v) :
    m_type(TYPE_INT),
    m_int(v)
  {}

  PropertyValue(float v) :
    m_type(TYPE_FLOAT),
    m_float(v)
  {}

  PropertyValue(const std::string& v) :
    m_type(TYPE_STRING),
    m_string(v)
  {}

  Type get_type() const
  {
    return m_type;
  }

  bool get_bool() const
  {
    return m_bool;
  }

  int get_int() const
  {
    return m_int;
  }

  float get_float() const
  {
    return m_float;
  }

  std::string get_string() const
  {
    return m_string;
  }
};

#endif

/* EOF */
