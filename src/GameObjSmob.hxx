//  $Id: GameObjSmob.hxx,v 1.1 2002/03/19 17:56:55 grumbel Exp $
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

#ifndef GAMEOBJSMOB_HXX
#define GAMEOBJSMOB_HXX

#include "GameObj.hxx"
#include "SCMConverter.hxx"

class GameObjSmob
{
private:
  static long tag;

  static scm_sizet free (SCM smob);
  static SCM mark (SCM);
  static int print (SCM image_smob, SCM port, scm_print_state *pstate);
public:
  static void register_guile_bindings ();
  static long get_smob_tag () { return tag; }

  static SCM get_world (SCM scm_object);
  static SCM get_data (SCM scm_object);
  static SCM set_data (SCM scm_object, SCM scm_data);
};

template<>
struct SmobInfo<GameObj> 
{
  typedef GameObjSmob smob_type;
  static long get_smob_tag () { return GameObjSmob::get_smob_tag (); }
};

#endif

/* EOF */
