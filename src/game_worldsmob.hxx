//  $Id: game_worldsmob.hxx,v 1.1 2003/08/10 19:56:40 grumbel Exp $
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

#ifndef GAMEWORLDSMOB_HXX
#define GAMEWORLDSMOB_HXX

#include <guile/gh.h>
#include "scmconverter.hxx"
#include "game_world.hxx"

class GameWorldSmob
{
private:
  static long tag;

  static scm_sizet free (SCM smob);
  static SCM mark (SCM);
  static int print (SCM image_smob, SCM port, scm_print_state *pstate);
public:
  static void register_guile_bindings ();
  static long get_smob_tag () { return tag; }

  static SCM add (SCM scm_world, SCM scm_object);
  static SCM remove (SCM scm_world, SCM scm_object);

  static SCM get_time (SCM scm_world);
  static SCM get_tilemap (SCM scm_world);
  static SCM get_objects (SCM scm_world);
};

template<>
struct SmobInfo<GameWorld> 
{
  typedef GameWorldSmob smob_type;
  static long get_smob_tag () { return GameWorldSmob::get_smob_tag (); }
};

#endif

/* EOF */
