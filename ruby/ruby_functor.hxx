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

#ifndef HEADER_RUBY_FUNCTOR_HXX
#define HEADER_RUBY_FUNCTOR_HXX

#include <iostream>
#include "ruby.h"
#include "ruby_object.hxx"
#include "flexlay_wrap.hxx"

/** */
class RubyFunctor
{
private:
  RubyObject val;

public:
  RubyFunctor(const RubyObject& val_);  
  ~RubyFunctor();

  void operator()();
  void operator()(int i);
  void operator()(int x, int y);

  /** Print backtrace in case of error */
  static void print_error();

  // FIXME: Protect these function calls somehow
  template<class C> void operator()(const C& c)
  {
    if (1) {
      rb_funcall(val.ptr(), rb_intern("call"), 1,
                 convert_to_ruby_value(c));
    } else {
      //VALUE arg1 = convert_to_ruby_value(c);
      //rb_funcall(val.ptr(), rb_intern("call"), 1, arg1);
      int state = 0;
      VALUE args[2];
      args[0] = reinterpret_cast<VALUE>(this);
      args[1] = convert_to_ruby_value(c);
      rb_protect(&RubyFunctor::funcall_protect1, reinterpret_cast<VALUE>(args), &state);
      if (state)
        print_error();
    }
  }

  template<class C, class D> void operator()(const C& c, const D& d)
  {
    if (1) {
      rb_funcall(val.ptr(), rb_intern("call"), 2,
                 convert_to_ruby_value(c),
                 convert_to_ruby_value(d));
    } else {
      std::cout << "Calling operator() with two args" << std::endl;
      int state = 0;
      VALUE args[2];
      args[0] = reinterpret_cast<VALUE>(this);
      args[1] = convert_to_ruby_value(c);
      args[2] = convert_to_ruby_value(d);
      rb_protect(&RubyFunctor::funcall_protect2, reinterpret_cast<VALUE>(args), &state);
      if (state)
        print_error();
    }
  }
  
  static VALUE funcall_protect(VALUE self);
  static VALUE funcall_protect1(VALUE self);
  static VALUE funcall_protect2(VALUE self);
};

#endif

/* EOF */
