//  Flexlay - A Generic 2D Game Editor
//  Copyright (C) 2000 Ingo Ruhnke <grumbel@gmx.de>
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

#ifndef HEADER_FLEXLAY_FIELD_HPP
#define HEADER_FLEXLAY_FIELD_HPP

#include <vector>
#include <algorithm>
#include <stdexcept>
#include <sstream>

template<class T>
class Field
{
private:
  int width;
  int height;
  std::vector<T> vec;

  static int checked_dim(int v, const char* name)
  {
    if (v < 0)
    {
      std::ostringstream msg;
      msg << "Field: " << name << " must be >= 0 (got " << v << ")";
      throw std::invalid_argument(msg.str());
    }
    return v;
  }

  void check_xy(int x, int y) const
  {
    if (!in_bounds(x, y))
    {
      std::ostringstream msg;
      msg << "Field: index (" << x << ", " << y << ") out of range for "
          << width << "x" << height;
      throw std::out_of_range(msg.str());
    }
  }

  void check_index(int i) const
  {
    if (i < 0 || i >= size())
    {
      std::ostringstream msg;
      msg << "Field: linear index " << i << " out of range for size " << size();
      throw std::out_of_range(msg.str());
    }
  }

public:
  typedef typename std::vector<T>::iterator iterator;

  Field()
    : width(0), height(0)
  {
  }

  Field(int w, int h)
    : width(checked_dim(w, "width")),
      height(checked_dim(h, "height")),
      vec(static_cast<typename std::vector<T>::size_type>(width) *
          static_cast<typename std::vector<T>::size_type>(height))
  {
  }

  Field(const Field<T>& copy)
    : width(copy.width), height(copy.height), vec(copy.vec)
  {
  }

  /** Creates a new field out of a subsection from an already existing one.
   *  @param pos_x The position of the old field in the new resized one
   *  @param pos_y The position of the old field in the new resized one */
  Field(const Field<T>& arg_field, int w, int h, int pos_x, int pos_y)
    : width(checked_dim(w, "width")),
      height(checked_dim(h, "height")),
      vec(static_cast<typename std::vector<T>::size_type>(width) *
          static_cast<typename std::vector<T>::size_type>(height))
  {
    int start_x = std::max(0, -pos_x);
    int start_y = std::max(0, -pos_y);

    int end_x = std::min(arg_field.get_width(),  get_width()  - pos_x);
    int end_y = std::min(arg_field.get_height(), get_height() - pos_y);

    for (int y = start_y; y < end_y; ++y)
      for (int x = start_x; x < end_x; ++x)
        at(pos_x + x, pos_y + y) = arg_field.at(x, y);
  }

  Field<T>& operator=(const Field<T>& copy)
  {
    if (this != &copy)
    {
      width  = copy.width;
      height = copy.height;
      vec    = copy.vec;
    }
    return *this;
  }

  bool in_bounds(int x, int y) const
  {
    return x >= 0 && x < width && y >= 0 && y < height;
  }

  const T& operator[](int i) const
  {
    check_index(i);
    return vec[static_cast<typename std::vector<T>::size_type>(i)];
  }

  T& operator[](int i)
  {
    check_index(i);
    return vec[static_cast<typename std::vector<T>::size_type>(i)];
  }

  T& operator()(int x, int y)
  {
    check_xy(x, y);
    return vec[static_cast<typename std::vector<T>::size_type>(width * y + x)];
  }

  const T& operator()(int x, int y) const
  {
    check_xy(x, y);
    return vec[static_cast<typename std::vector<T>::size_type>(width * y + x)];
  }

  /** Unchecked access for hot paths after the caller has proven bounds. */
  T& at_unchecked(int x, int y)
  {
    return vec[static_cast<typename std::vector<T>::size_type>(width * y + x)];
  }

  const T& at_unchecked(int x, int y) const
  {
    return vec[static_cast<typename std::vector<T>::size_type>(width * y + x)];
  }

  inline const T& at(int x, int y) const
  {
    return (*this)(x, y);
  }

  inline T& at(int x, int y)
  {
    return (*this)(x, y);
  }

  /** Resize a field to a new size
   *  @param pos_x The position of the old field in the new resized one
   *  @param pos_y The position of the old field in the new resized one
   **/
  void resize(int w, int h, int pos_x = 0, int pos_y = 0)
  {
    *this = Field<T>(*this, w, h, pos_x, pos_y);
  }

  void clear()
  {
    width  = 0;
    height = 0;
    vec.clear();
  }

  std::vector<T>& get_data() { return vec; }
  const std::vector<T>& get_data() const { return vec; }

  /** Copy elements from d.  If sizes differ, copies
      min(size(), d.size()) elements; remaining cells are unchanged.
      Callers that require an exact match should compare sizes first. */
  void set_data(const std::vector<T>& d)
  {
    const typename std::vector<T>::size_type n =
      std::min(vec.size(), d.size());
    for (typename std::vector<T>::size_type i = 0; i < n; ++i)
      vec[i] = d[i];
  }

  iterator begin() { return vec.begin(); }
  iterator end()   { return vec.end(); }

  int size() const { return static_cast<int>(vec.size()); }
  int get_width()  const { return width; }
  int get_height() const { return height; }
};

#endif

/* EOF */
