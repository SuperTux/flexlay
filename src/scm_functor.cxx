//  $Id: scm_functor.cxx,v 1.1 2003/09/10 08:25:29 grumbel Exp $
//
//  Feuerkraft - A Tank Battle Game
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
#include <libguile.h>
#include "scm_functor.hxx"

SCMFunctor::SCMFunctor(SCM arg_func)
{
  func = arg_func;
  scm_gc_protect_object(func);
}

SCMFunctor::~SCMFunctor()
{
  scm_gc_unprotect_object(func);
}

SCMFunctor::SCMFunctor(const SCMFunctor& hook)
{
  func = hook.func;

  scm_gc_protect_object(func);
}

SCMFunctor& 
SCMFunctor::operator= (const SCMFunctor& hook)
{
  if (this != &hook)
    {
      scm_gc_unprotect_object(func);

      func = hook.func;

      scm_gc_protect_object(func);
    }
  return *this;
}

void
SCMFunctor::operator()()
{
  scm_call_0(func);
  //SCM lst = scm_listify(scm_sym_lambda, SCM_EOL, scm_listify(func, SCM_UNDEFINED), SCM_UNDEFINED);
  //SCM wrapped_func = scm_primitive_eval(lst);
  //scm_catch(SCM_BOOL_T, wrapped_func, scm_variable_ref(scm_c_lookup("*error-handler*")));
}

void
SCMFunctor::operator()(SCM arg)
{
  scm_call_1(func, arg);
}

void
SCMFunctor::operator()(SCM arg1, SCM arg2)
{
  scm_call_2(func, arg1, arg2);
}

void
SCMFunctor::operator()(SCM arg1, SCM arg2, SCM arg3)
{
  scm_call_3(func, arg1, arg2, arg3);
}

/* EOF */
