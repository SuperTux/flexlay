//  $Id: tile_mapsmob.cxx,v 1.1 2003/08/10 19:56:40 grumbel Exp $
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

#include "tile_mapsmob.hxx"

long TileMapSmob::tag;

scm_sizet
TileMapSmob::free (SCM smob)
{
  return 0;
}

SCM
TileMapSmob::mark (SCM)
{
  return SCM_BOOL_F;
}

int
TileMapSmob::print (SCM image_smob, SCM port, scm_print_state *pstate)
{
  scm_puts ("TileMap", port);
  return 0;
}

void
TileMapSmob::register_guile_bindings ()
{
  tag = scm_make_smob_type ("TileMap", 0);

  scm_set_smob_mark  (tag, &TileMapSmob::mark);
  scm_set_smob_free  (tag, &TileMapSmob::free);
  scm_set_smob_print (tag, &TileMapSmob::print);

  gh_new_procedure3_0 ("tilemap:is-ground", &TileMapSmob::is_ground);
}

SCM
TileMapSmob::is_ground (SCM scm_tilemap, SCM scm_x_pos, SCM scm_y_pos)
{
  TileMap* tilemap = checked_smob_cast<TileMap>(scm_tilemap);
  
  return gh_bool2scm (tilemap->is_ground ((int) gh_scm2double (scm_x_pos), 
					  (int) gh_scm2double (scm_y_pos)));
}

/* EOF */
