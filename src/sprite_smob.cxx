//  $Id: sprite_smob.cxx,v 1.2 2003/08/12 08:24:41 grumbel Exp $
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

#include "scmconverter.hxx"
#include <ClanLib/gl.h>
#include <ClanLib/display.h>
#include "sprite_smob.hxx"
#include "globals.hxx"

long SpriteSmob::tag;

void
SpriteSmob::register_guile_bindings ()
{
  tag = scm_make_smob_type ("SpriteSmob", 0);

  scm_set_smob_mark  (tag, SpriteSmob::mark);
  scm_set_smob_free  (tag, SpriteSmob::free);
  scm_set_smob_print (tag, SpriteSmob::print);

  gh_new_procedure1_0 ("sprite:create", &SpriteSmob::create);
  gh_new_procedure3_0 ("sprite:set-hotspot", &SpriteSmob::set_hotspot);
  gh_new_procedure3_0 ("sprite:draw", &SpriteSmob::draw);
  gh_new_procedure2_0 ("sprite:update", &SpriteSmob::update);
}

scm_sizet
SpriteSmob::free (SCM smob) 
{
  //SpriteSmob* sprite_smob = unchecked_smob_cast<SpriteSmob>(smob);
  //delete sprite_smob;
  return 0; //sizeof (SpriteDrawable);
}


SCM
SpriteSmob::mark (SCM)
{
  return SCM_UNSPECIFIED;
}

int
SpriteSmob::print (SCM image_smob, SCM port, scm_print_state *pstate)
{
  scm_puts ("#<c:SpriteSmob>", port);
  return 1;
}

SCM
SpriteSmob::update (SCM scm_sprite, SCM scm_delta)
{
  CL_Sprite* sprite = checked_smob_cast<CL_Sprite> (tag, scm_sprite);
  float delta = gh_scm2double (scm_delta);

  sprite->update (delta);
  return SCM_UNSPECIFIED;
}

SCM
SpriteSmob::draw (SCM scm_sprite, SCM scm_x_pos, SCM scm_y_pos)
{
  CL_Sprite* sprite = checked_smob_cast<CL_Sprite> (tag, scm_sprite);  

  int x_pos = (int) gh_scm2double (scm_x_pos);
  int y_pos = (int) gh_scm2double (scm_y_pos);

  //glBlendFunc(GL_ONE, GL_ONE);
  sprite->draw (x_pos, y_pos);
  //glBlendFunc(GL_ONE, GL_ZERO);

  return SCM_UNSPECIFIED;
}

SCM
SpriteSmob::set_hotspot (SCM scm_sprite, SCM scm_x_pos, SCM scm_y_pos)
{
  CL_Sprite* sprite = checked_smob_cast<CL_Sprite> (tag, scm_sprite);
  int x_pos = (int) gh_scm2double (scm_x_pos);
  int y_pos = (int) gh_scm2double(scm_y_pos);

  sprite->set_alignment (origin_center, x_pos, y_pos);
  return SCM_UNSPECIFIED;
}

SCM
SpriteSmob::create (SCM scm_name)
{
  CL_Sprite* sprite;
  try {
    std::cout << "Creating sprite and making a memory leak..." << std::endl;
    sprite = new CL_Sprite (SCM_CHARS (scm_name), resources);
  } catch (CL_Error& error) {
    std::cout << "CL_Error: " << error.message << std::endl;
    assert (0);
  }
  SCM_RETURN_NEWSMOB (tag, sprite); 
}

/* EOF */
