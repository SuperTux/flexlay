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

#ifndef HEADER_SHARED_PTR_HXX
#define HEADER_SHARED_PTR_HXX

#include <iostream>
#include <typeinfo>
 
template<class T>
class SharedPtrDeleter
{
public:
  T* ptr; 

  SharedPtrDeleter(T* p) : ptr(p) {}
  virtual ~SharedPtrDeleter() {}
  
  virtual void del() =0;
  virtual SharedPtrDeleter* clone() =0;
};

template<class T>
class SharedPtrDeleterImpl : public SharedPtrDeleter<T>
{
public:
  SharedPtrDeleterImpl(T* p)
    : SharedPtrDeleter<T>(p) {}

  ~SharedPtrDeleterImpl()
  {
  }  

  void del() {
    //delete ptr;
    ptr = 0;
  }

  SharedPtrDeleter<T>* clone() {
    return new SharedPtrDeleterImpl<T>(ptr);
  }
};

template<class T>
class SharedPtr
{
private:
  SharedPtrDeleter<T>* deleter;
  int* ref_count;

  void inc() {
    *ref_count += 1;
  }
  
  void dec() {
    *ref_count -= 1;
    if (*ref_count == 0) {
      std::cout << "SharedPtr: deleting: " << typeid(deleter->ptr).name() << std::endl;
      deleter->del();
      delete ref_count;
    }
  }
public:
  template<class Base> friend class SharedPtr;

  SharedPtr()
    : deleter(new SharedPtrDeleterImpl<T>(0)), 
      ref_count(new int(1))
  {}

  template<typename D>
  SharedPtr(D* p)
    : deleter(new SharedPtrDeleterImpl<T>(p)), 
      ref_count(new int(1))
  {
  }
  
  template<class Base>
  SharedPtr(const SharedPtr<Base>& copy)
  {
    deleter   = new SharedPtrDeleterImpl<T>(copy.deleter->ptr);
    ref_count = copy.ref_count;
    inc();
  }

  template<class Base>
  SharedPtr<T>& operator= (const SharedPtr<Base>& copy) 
  {
    dec();
    //delete deleter;
    deleter   = new SharedPtrDeleterImpl<T>(copy.deleter->ptr);
    ref_count = copy.ref_count;
    inc();

    return *this;
  }
  
  ~SharedPtr()
  {
    dec();
    //delete deleter;
  }

  //: Dereferencing operator.
  T& operator*() { return *deleter->ptr; }

  T const& operator*() const { return *deleter->ptr; }
	
  //: Indirect member access operator.
  T* operator->() { return deleter->ptr; }

  T const* operator->() const { return deleter->ptr; }

  T* get() const { return deleter->ptr; }
};

#endif

/* EOF */
