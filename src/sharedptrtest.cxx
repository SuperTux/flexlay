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
#include <ClanLib/Core/System/sharedptr.h>
#include "sharedptrtest.hxx"

class B
{
public:
  B() { std::cout << "B(" << this << ")" << std::endl; }
  virtual ~B() { std::cout << "~B(" << this << ")" << std::endl; }
};

class A : public B
{
public:
  A() { std::cout << "A(" << this << ")" << std::endl; }
  virtual ~A() { std::cout << "~A(" << this << ")" << std::endl; }
};

class C;

int main()
{
  SharedPtr<A> p0_;
  SharedPtr<A> p1_;
  SharedPtr<B> p2_(p0_);
  SharedPtr<B> p3_(p1_);

  SharedPtr<B> p0(new B());
  {
    SharedPtr<B> ptr0(new A());
  }
  {
    SharedPtr<A> aptr0(new A());
    {
      p0_ = p1_;
      SharedPtr<B> ptr1(new B());
      {
        SharedPtr<B> ptr2 = ptr1;
        SharedPtr<B> ptr1 = ptr2;
        ptr1 = aptr0;
        p0   = aptr0;
      }
    }
  }
}

/* EOF */
