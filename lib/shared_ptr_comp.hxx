//  $Id$
// 
//  Flexlay - A Generic 2D Game Editor
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
    //if (ptr)
    //  delete ptr;
    ptr = 0;
  }
};

template<class T>
class SharedPtr
{
private:
  SharedPtrDeleter<T>* deleter;
  int* ref_count;

  void inc() {
#ifdef DEBUG
    std::cout << "SharedPtr: inc: " << (ref_count ? *ref_count : -45) << std::endl;
#endif
    if (ref_count)
      {
        *ref_count += 1;
      }
  }
  
  void dec() {
#ifdef DEBUG
    std::cout << "SharedPtr: dec: " << (ref_count ? *ref_count : -45) << std::endl;
#endif
    if (ref_count)
      {
        *ref_count -= 1;
        if (*ref_count == 0) {
#ifdef DEBUG
          std::cout << "SharedPtr: deleting: type: "
                    << typeid(deleter->ptr).name()
                    << " ptr: " << deleter->ptr
                    << std::endl;
#endif
          deleter->del();
          
          delete ref_count; ref_count = 0;
          delete deleter;   deleter   = 0;
        }
      }
    else
      {
#ifdef DEBUG
        std::cout << "SharedPtr: null delete" << std::endl;
#endif
      }
  }
public:
  template<class Base> friend class SharedPtr;

  // Constructors
  SharedPtr()
    : deleter(0),
      ref_count(0)
  {
#ifdef DEBUG
    std::cout << "SharedPtr: ctor null" << std::endl;
#endif
  }

  template<typename D>
  SharedPtr(D* p)
    : deleter(new SharedPtrDeleterImpl<T>(p)), 
      ref_count(new int(1))
  {
#ifdef DEBUG
    std::cout << "SharedPtr: ctor: type: "
              << typeid(deleter->ptr).name()
              << " ptr: " << deleter->ptr
              << std::endl;
#endif
  }
  
  template<class Base>
  SharedPtr(const SharedPtr<Base>& copy)
    : deleter(0), ref_count(0)
  {
    if (copy.deleter)
      {
        deleter   = new SharedPtrDeleterImpl<T>(copy.deleter->ptr);
        ref_count = copy.ref_count;
        inc();
      }

#ifdef DEBUG
    if (deleter)
      {
        std::cout << "SharedPtr: copy-ctor template: type: "
                  << typeid(deleter->ptr).name()
                  << " ptr: " << deleter->ptr
                  << std::endl;
      }
    else
      {
        std::cout << "SharedPtr: copy-ctor template null" << std::endl;
      }
#endif
  }

  // Assign
  template<class Base>
  SharedPtr<T>& operator= (const SharedPtr<Base>& copy) 
  {
#ifdef DEBUG
    std::cout << "SharedPtr<T>& operator= (const SharedPtr<Base>& copy)" << std::endl;
#endif
    if (ref_count != copy.ref_count)
      {
        dec();

        if (copy.deleter)
          {
            deleter   = new SharedPtrDeleterImpl<T>(copy.deleter->ptr);
            ref_count = copy.ref_count;
            inc();
          }

#ifdef DEBUG
        if (deleter)
          {
            std::cout << "SharedPtr: assign template: type: "
                      << typeid(deleter->ptr).name()
                      << " ptr: " << deleter->ptr
                      << std::endl;
          }
        else
          {
            std::cout << "SharedPtr: assign template: null: " << std::endl;
          }
#endif
      }

    return *this;
  }

  SharedPtr<T>& operator= (const SharedPtr<T>& copy) 
  {
    if (this != &copy)
      {
        dec();

        if (copy.deleter)
          {
            deleter   = new SharedPtrDeleterImpl<T>(copy.deleter->ptr);
            ref_count = copy.ref_count;
            inc();
          }

#ifdef DEBUG
        if (deleter)
          {
            std::cout << "SharedPtr: assign normal: type: "
                      << typeid(deleter->ptr).name()
                      << " ptr: " << deleter->ptr
                      << std::endl;
          }
        else
          {
            std::cout << "SharedPtr: assign normal null" << std::endl;
          }
#endif
      }
    else
      {
#ifdef DEBUG
        if (deleter)
          {
            std::cout << "SharedPtr: self assin: type: "
                      << typeid(deleter->ptr).name()
                      << " ptr: " << deleter->ptr
                      << std::endl;
          }
        else
          {
            std::cout << "SharedPtr: assign normal null" << std::endl;
          }
#endif
      }

    return *this;
  }
  
  ~SharedPtr()
  {
    dec();
  }

  //: Dereferencing operator.
  T& operator*() { return *deleter->ptr; }

  T const& operator*() const { return *deleter->ptr; }
	
  //: Indirect member access operator.
  T* operator->() { return deleter->ptr; }

  T const* operator->() const { return deleter->ptr; }

  T* get() const 
  {
    if (deleter) 
      return deleter->ptr;
    else
      return 0; 
  }
};

#endif

/* EOF */
