//  $Id: tile_mapsmob.hxx,v 1.1 2003/08/10 19:56:40 grumbel Exp $
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

#ifndef TILEMAPSMOB_HXX
#define TILEMAPSMOB_HXX

#include "scmconverter.hxx"
#include "tile_map.hxx"

class TileMapSmob
{
private:
  static long tag;

  static scm_sizet free (SCM smob);
  static SCM mark (SCM);
  static int print (SCM image_smob, SCM port, scm_print_state *pstate);

public:
  static long get_smob_tag () { return tag; }
  static void register_guile_bindings (); 

  static SCM is_ground (SCM scm_tilemap, SCM scm_x_pos, SCM scm_y_pos);
};

template<>
struct SmobInfo<TileMap> 
{
  typedef TileMapSmob smob_type;
  static long get_smob_tag () { return TileMapSmob::get_smob_tag (); }
};

#endif

/* EOF */
