//  $Id: Field.hxx,v 1.1 2002/03/19 17:56:55 grumbel Exp $
// 
//  Pingus - A free Lemmings clone
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
  unsigned int width;
  unsigned int height;
  std::vector<T> vec;

public:
  typedef std::vector<T>::iterator iterator;

  Field (unsigned int w, unsigned int h) 
    : width (w), height (h), vec (width * height)
  {
  }

  T& operator() (int x, int y) 
  {
    assert (x >= 0 || x < (int) width || y >= 0 || y < (int) height);
    return vec [width*y + x];
  }
  
  T& at (int x, int y) {
    return (*this) (x, y);
  }

  iterator begin () { return vec.begin (); }
  iterator end () { return vec.end (); }

  unsigned int get_width () const { return width; }
  unsigned int get_height () const { return height; }
};

#endif

/* EOF */
