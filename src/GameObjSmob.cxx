//  $Id: GameObjSmob.cxx,v 1.1 2002/03/19 17:56:55 grumbel Exp $
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

#include "GameWorldSmob.hxx"
#include "GameObjSmob.hxx"
#include "GuileGameObj.hxx"

long GameObjSmob::tag;

scm_sizet
GameObjSmob::free (SCM smob)
{
  return 0;
}

SCM
GameObjSmob::mark (SCM)
{
  return SCM_BOOL_F;
}

int
GameObjSmob::print (SCM image_smob, SCM port, scm_print_state *pstate)
{
  scm_puts ("GameObj", port);
  return 0;
}

void
GameObjSmob::register_guile_bindings ()
{
  tag = scm_make_smob_type ("GameObj", 0);
  
  scm_set_smob_mark  (tag, &GameObjSmob::mark);
  scm_set_smob_free  (tag, &GameObjSmob::free);
  scm_set_smob_print (tag, &GameObjSmob::print);

  gh_new_procedure1_0 ("gameobj:get-world", &GameObjSmob::get_world);
  gh_new_procedure1_0 ("gameobj:get-data", &GameObjSmob::get_data);
  gh_new_procedure2_0 ("gameobj:set-data", &GameObjSmob::set_data);
}

SCM
GameObjSmob::get_world (SCM scm_object)
{
  return create_smob (checked_smob_cast<GameObj>(scm_object)->get_world ());
}

SCM
GameObjSmob::get_data (SCM scm_object)
{
  GameObj*      tmp_obj = checked_smob_cast<GameObj>(scm_object);
  GuileGameObj* obj = dynamic_cast<GuileGameObj*>(tmp_obj);

  if (obj)
    return obj->get_data ();
  else
    return SCM_BOOL_F;
}

SCM
GameObjSmob::set_data (SCM scm_object, SCM scm_data)
{
  GameObj*      tmp_obj = checked_smob_cast<GameObj>(scm_object);
  GuileGameObj* obj = dynamic_cast<GuileGameObj*>(tmp_obj);

  if (obj)
    return obj->set_data (scm_data);
  else
    return SCM_BOOL_F;
}

/* EOF */
