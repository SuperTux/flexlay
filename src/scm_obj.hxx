//  $Id: SCMObj.hxx,v 1.1 2001/09/20 20:01:29 grumbel Exp $
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

#ifndef SCMOBJ_HXX
#define SCMOBJ_HXX

#include <guile/gh.h>

/** SCM Wrapper class which handles protect/unprotect operations. Use
    this for SCM which are on the heap and not for SCM's on the
    stack */
class SCMObj
{
private:
  SCM obj;

public:
  explicit SCMObj () {
    obj = SCM_BOOL_F;
    scm_protect_object (obj);
  }

  explicit SCMObj (SCM new_obj) {
    obj = new_obj;
    scm_protect_object (obj);
  }

  explicit SCMObj (const SCMObj& new_obj) {
    obj = new_obj.obj;
    scm_protect_object (obj);
  }

  ~SCMObj () {
    scm_unprotect_object (obj);
  }
  
  SCMObj& operator=(const SCMObj& new_obj) {
    scm_unprotect_object (obj);
    obj = new_obj.obj;
    scm_protect_object (obj);
    return *this;
  }

  SCM get_scm () const {
    return obj;
  }
};

#endif

/* EOF */
