//  $Id: animation_obj.cxx,v 1.2 2003/08/11 21:50:35 grumbel Exp $
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

#include <ClanLib/display.h>
#include "globals.hxx"
#include "animation_obj.hxx"

extern CL_ResourceManager* resources;

AnimationObj::AnimationObj (const std::string& str, const CL_Vector& arg_pos)
  : sprite(CL_Sprite (str, resources)), 
    pos(arg_pos)
{
}

void
AnimationObj::draw ()
{
  sprite.draw (int (pos.x), int (pos.y));
}

void
AnimationObj::update (float delta)
{
  if (sprite.is_finished ())
    remove ();
  sprite.update (delta);
}

/* EOF */
