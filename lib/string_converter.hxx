//  $Id: string_converter.hxx,v 1.3 2003/11/04 22:48:51 grumbel Exp $
// 
//  Flexlay - A Generic 2D Game Editor
//  Copyright (C) 2000 Ingo Ruhnke <grumbel@gmx.de>
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

#ifndef STRINGCONVERTER_HXX
#define STRINGCONVERTER_HXX

#include <stdexcept>
#include <sstream>
#include "config.h"

template <class T>
std::string to_string(const T& any)
{
  std::ostringstream oss;
  oss << any;
  return oss.str();
}

template <class T>
bool from_string(const std::string& rep, T& x)
{
  // this is necessary so that if "x" is not modified if the conversion fails
  T temp;
  std::istringstream iss(rep);

  iss >> temp;

  if (iss.fail()) {
    return false;
  } else {
    x = temp;
    return true;
  }
}

inline bool has_suffix(const std::string& data, const std::string& suffix)
{
  if (data.length() >= suffix.length())
    return data.compare(data.length() - suffix.length(), suffix.length(), suffix) == 0;
  else
    return false;
}

#endif

/* EOF */
