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

#include "ruby_functor.hxx"

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
  rb_funcall(val.ptr(), rb_intern("call"), 0);
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

/* EOF */
