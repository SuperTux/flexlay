//  $Id: GameWorldSmob.cxx,v 1.1 2002/03/19 17:56:55 grumbel Exp $
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

#include "GameObjSmob.hxx"
#include "GameWorldSmob.hxx"
#include "TileMapSmob.hxx"

long GameWorldSmob::tag;

scm_sizet
GameWorldSmob::free (SCM smob)
{
  return 0;
}

SCM
GameWorldSmob::mark (SCM)
{
  return SCM_BOOL_F;
}

int
GameWorldSmob::print (SCM image_smob, SCM port, scm_print_state *pstate)
{
  scm_puts ("GameWorld", port);
  return 0;
}

void
GameWorldSmob::register_guile_bindings ()
{
  tag = scm_make_smob_type ("GameWorld", 0);
  
  scm_set_smob_mark  (tag, &GameWorldSmob::mark);
  scm_set_smob_free  (tag, &GameWorldSmob::free);
  scm_set_smob_print (tag, &GameWorldSmob::print);

  //gh_new_procedure1_0 ("gameobj:get-world", &GameObjSmob::get_world);
  gh_new_procedure1_0 ("gameworld:get-tilemap", &GameWorldSmob::get_tilemap);
  gh_new_procedure2_0 ("gameworld:add", &GameWorldSmob::add);
  gh_new_procedure2_0 ("gameworld:remove", &GameWorldSmob::remove);
  gh_new_procedure1_0 ("gameworld:get-time", &GameWorldSmob::get_time);
}

SCM
GameWorldSmob::add (SCM scm_world, SCM scm_object)
{
  GameWorld* world = checked_smob_cast<GameWorld>(scm_world);
  world->add (checked_smob_cast<GameObj>(scm_object));
  return SCM_UNSPECIFIED;
}

SCM
GameWorldSmob::remove (SCM scm_world, SCM scm_object)
{
  GameWorld* world = checked_smob_cast<GameWorld>(scm_world);
  world->remove (checked_smob_cast<GameObj>(scm_object));
  return SCM_UNSPECIFIED;
}

SCM
GameWorldSmob::get_time (SCM scm_world)
{
  GameWorld* world = checked_smob_cast<GameWorld>(scm_world);
  return gh_double2scm(world->get_time ());
}

SCM
GameWorldSmob::get_tilemap (SCM scm_world)
{
  GameWorld* world = checked_smob_cast<GameWorld>(scm_world);
  return create_smob<TileMap>(world->get_tilemap ());
}

SCM
GameWorldSmob::get_objects (SCM scm_world)
{
  GameWorld* world = checked_smob_cast<GameWorld>(scm_world);
  //world->get_objects ()
  assert (!"not implemented");
  return SCM_UNSPECIFIED;
}

/* EOF */
