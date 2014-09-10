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

#include "ruby_object.hpp"

RubyObject::RubyObject(VALUE val_)
  : val(val_)
{
  rb_gc_register_address(&val);
}

RubyObject::RubyObject(const RubyObject& copy)
  : val(copy.val)
{
  rb_gc_register_address(&val);
}

RubyObject&
RubyObject::operator= (const RubyObject& copy)
{
  if (this != &copy)
    {
      rb_gc_unregister_address(&val);
      val = copy.val;
      rb_gc_register_address(&val);
    }
  return *this;
}

RubyObject::~RubyObject()
{
  rb_gc_unregister_address(&val);
}

VALUE
RubyObject::ptr()
{
  return val;
}

/* EOF */
