//  $Id: guile_gameobj_factory.hxx,v 1.1 2003/08/10 19:56:40 grumbel Exp $
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

#ifndef GUILEGAMEOBJFACTORY_HXX
#define GUILEGAMEOBJFACTORY_HXX

#include <string>
#include <map>
#include <guile/gh.h>
#include "guile_gameobj.hxx"

class GuileGameObjDesc
{
public:
  SCM scm_create;
  SCM scm_update;
  SCM scm_draw;

  GuileGameObjDesc () {
    scm_create = scm_update = scm_draw = SCM_BOOL_F;
  }

  GuileGameObjDesc (SCM arg_create, SCM arg_update, SCM arg_draw) 
    : scm_create (arg_create), scm_update (arg_update), scm_draw (arg_draw)
  {
    scm_protect_object (scm_create);
    scm_protect_object (scm_update);
    scm_protect_object (scm_draw);
  }
};

class GuileGameObjFactory
{
private:
  typedef std::map <std::string, GuileGameObjDesc> Descr;
  static Descr descriptions;

public:
  static void register_guile_bindings ();
  static GuileGameObj* create (const std::string& name);

  static SCM register_object (SCM name, SCM arg_struct);
};

#endif

/* EOF */
