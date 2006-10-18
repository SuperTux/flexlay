//  $Id: field.hxx,v 1.4 2003/09/12 09:25:48 grumbel Exp $
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

#ifndef FIELD_HXX
#define FIELD_HXX

#include <vector>
#include <assert.h>

template<class T>
class Field
{
private:
  int width;
  int height;
  std::vector<T> vec;

public:
  typedef typename std::vector<T>::iterator iterator;

  Field()
    : width(0), height(0)
  {
  }

  Field (int w, int h) 
    : width (w), height (h), vec (width * height)
  {
  }

  Field(const Field<T>& copy)
    : width(copy.width), height(copy.height), vec(copy.vec)
  {
  }

  /** Creates a new field out of a subsection from an already excisting one 
   *  @param pos_x The position of the old field in the new resized one
   *  @param pos_y The position of the old field in the new resized one */
  Field(const Field<T>& arg_field, int w, int h, int pos_x, int pos_y)
    : width (w), height (h), vec (width * height)
  {
    int start_x = std::max(0, -pos_x);
    int start_y = std::max(0, -pos_y);

    int end_x = std::min(arg_field.get_width(),  get_width()  - pos_x);
    int end_y = std::min(arg_field.get_height(), get_height() - pos_y);

    for(int y = start_y; y < end_y; ++y)
      for(int x = start_x; x < end_x; ++x)
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

  const T& operator[] (int i) const {
    return vec[i];
  }

  T& operator[] (int i) {
    return vec[i];
  }

  T& operator() (int x, int y) 
  {
    assert (x >= 0 || x < (int) width || y >= 0 || y < (int) height);
    return vec [width*y + x];
  }

  const T& operator() (int x, int y) const
  {
    assert (x >= 0 || x < (int) width || y >= 0 || y < (int) height);
    return vec [width*y + x];
  }
  
  inline const T& at (int x, int y) const {
    return vec [width*y + x];
  }

  inline T& at (int x, int y) {
    return (*this) (x, y);
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
  void set_data(const std::vector<T>& d) { 
    for(typename std::vector<T>::size_type i = 0; i < vec.size() && i < d.size(); ++i)
      vec[i] = d[i]; 
  }

  iterator begin () { return vec.begin (); }
  iterator end () { return vec.end (); }

  int size() const { return vec.size(); }
  int get_width () const { return width; }
  int get_height () const { return height; }
};

#endif

/* EOF */
