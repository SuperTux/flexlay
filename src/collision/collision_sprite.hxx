//  $Id: collision_sprite.hxx,v 1.1 2003/09/02 22:05:02 grumbel Exp $
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

#ifndef HEADER_COLLISION_SPRITE_HXX
#define HEADER_COLLISION_SPRITE_HXX

#include <vector>

class CL_ResourceManager;
class CollisionMask;

/** A higher level version of the CollisionMask. It mimiks a bit of
    CL_Sprite so that both can be used with each other */
class CollisionSprite
{
private:
  typedef std::vector<CollisionMask*> Masks;
  Masks masks;
public:
  CollisionSprite();
  /** Generate a CollisionSprite by a given sprite description */
  CollisionSprite(const std::string& resource_id, CL_ResourceManager* resources);
  ~CollisionSprite();

  CollisionMask* get_frame(int frame);
private:
  CollisionSprite (const CollisionSprite&);
  CollisionSprite& operator= (const CollisionSprite&);
};

#endif

/* EOF */
