//  $Id: collision_sprite.cxx,v 1.1 2003/09/02 22:05:02 grumbel Exp $
//
//  Pingus - A free Lemmings clone
//  Copyright (C) 2002 Ingo Ruhnke <grumbel@gmx.de>
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

#include <ClanLib/Display/sprite_description.h>
#include "collision_mask.hxx"
#include "collision_sprite.hxx"

CollisionSprite::CollisionSprite()
{
}

CollisionSprite::CollisionSprite(const std::string& resource_id, CL_ResourceManager* resources)
{
  CL_SpriteDescription desc(resource_id, resources);

  std::list<CL_SpriteDescription::FramePair> frames = desc.get_frames();

  for (std::list<CL_SpriteDescription::FramePair>::iterator i = frames.begin(); 
       i != frames.end(); ++i)
    {
      masks.push_back(new CollisionMask(i->first));
    }
}

CollisionSprite::~CollisionSprite()
{
  for(Masks::iterator i = masks.begin(); i != masks.end(); ++i)
    delete *i;
}

CollisionMask*
CollisionSprite::get_frame(int frame)
{
  return masks[frame];
}

/* EOF */
