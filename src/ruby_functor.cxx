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

#include <iostream>
#include "flexlay_wrap.hxx"
#include "ruby_functor.hxx"

VALUE
RubyFunctor::call_protected(VALUE self)
{
  return rb_funcall(reinterpret_cast<RubyFunctor*>(self)->val.ptr(), rb_intern("call"), 0);
}

RubyFunctor::RubyFunctor(const RubyObject& val_)
  : val(val_)
{
}

RubyFunctor::~RubyFunctor()
{
}

void
RubyFunctor::operator()()
{
  int state = 0;
  rb_protect(&RubyFunctor::call_protected, reinterpret_cast<VALUE>(this), &state);
  if (state)
    {
      // FIXME: Potential memory leak
      std::cout << "######################################################" << std::endl;
      std::cout << "RubyException: " 
                << rb_str2cstr(rb_inspect(ruby_errinfo), 0) 
                << std::endl;

      VALUE trace = rb_funcall(ruby_errinfo, rb_intern("backtrace"), 0);
      for (int i = 0; i < RARRAY(trace)->len; ++i)
        std::cout << rb_str2cstr(rb_ary_entry(trace, i), 0) << std::endl;
      std::cout << "######################################################" << std::endl;
      ruby_errinfo = Qnil;
    }
}

void
RubyFunctor::operator()(int i)
{
  rb_funcall(val.ptr(), rb_intern("call"), 1, INT2FIX(i));
}

void
RubyFunctor::operator()(int x, int y)
{
  rb_funcall(val.ptr(), rb_intern("call"), 2, INT2FIX(x), INT2FIX(y));
}

void
RubyFunctor::operator()(ObjMapObject obj)
{
  VALUE argval = ObjMapObject2Value(obj);
  rb_funcall(val.ptr(), rb_intern("call"), 1, argval);
}

/* EOF */
