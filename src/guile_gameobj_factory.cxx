//  $Id: guile_gameobj_factory.cxx,v 1.2 2003/08/12 08:24:41 grumbel Exp $
//
//  Windstille - A Jump'n Shoot Game
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

#include "guile_gameobj_factory.hxx"

GuileGameObjFactory::Descr GuileGameObjFactory::descriptions;

void
GuileGameObjFactory::register_guile_bindings ()
{
  gh_new_procedure2_0 ("gameobj-factory:register", &GuileGameObjFactory::register_object);
}

GuileGameObj*
GuileGameObjFactory::create (const std::string& name)
{
  Descr::iterator i = descriptions.find (name);
  
  if (i == descriptions.end ())
    {
      assert (!"GuileObject not found");
      return 0;
    }
  else 
    {
      return new GuileGameObj (gh_call0 (i->second.scm_create),
			       i->second.scm_update,
			       i->second.scm_draw);
    }
}

SCM
GuileGameObjFactory::register_object (SCM name, SCM vec)
{
  descriptions[SCM_CHARS (name)] = GuileGameObjDesc (gh_vector_ref (vec, gh_int2scm(0)),
						     gh_vector_ref (vec, gh_int2scm(1)),
						     gh_vector_ref (vec, gh_int2scm(2)));
  return SCM_UNSPECIFIED;
}

/* EOF */
