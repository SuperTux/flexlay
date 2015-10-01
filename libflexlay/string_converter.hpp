// Flexlay - A Generic 2D Game Editor
// Copyright (C) 2000 Ingo Ruhnke <grumbel@gmail.com>
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

#ifndef HEADER_FLEXLAY_STRING_CONVERTER_HPP
#define HEADER_FLEXLAY_STRING_CONVERTER_HPP

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
