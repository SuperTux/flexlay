//  $Id: assert.hxx,v 1.4 2003/06/20 20:54:23 grumbel Exp $
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

#ifndef HEADER_ASSERT_HXX
#define HEADER_ASSERT_HXX

#include <assert.h>
#include <stdlib.h>
#include <iostream>

// A collection of assert helper functions

#ifdef NDEBUG
#  define AssertMsg(assert, message)
#  define Bailout(message)
#else 
#  define AssertMsg(assert, message) \
  if (assert) \
  { \
  } \
  else \
  { \
    std::cout << "!!!!!!!!!!!!! Assert !!!!!!!!!!!!!!!!!!\n" \
      << __FILE__ << ":" << __LINE__ << ": assertion '" << #assert << "' failed" << std::endl \
      << "Func: " << __PRETTY_FUNCTION__ << std::endl << "Msg:  " << message << std::endl; \
    exit(EXIT_FAILURE); \
  }
#  define Bailout(message) assert(!message)
#endif

#endif

/* EOF */
