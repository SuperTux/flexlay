//  Flexlay - A Generic 2D Game Editor
//  Copyright (C) 2002 Ingo Ruhnke <grumbel@gmx.de>
//
//  This program is free software: you can redistribute it and/or modify
//  it under the terms of the GNU General Public License as published by
//  the Free Software Foundation, either version 3 of the License, or
//  (at your option) any later version.
//  
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//  GNU General Public License for more details.
//  
//  You should have received a copy of the GNU General Public License
//  along with this program.  If not, see <http://www.gnu.org/licenses/>.

#ifndef HEADER_FLEXLAY_OBJMAP_SPRITE_OBJECT_HPP
#define HEADER_FLEXLAY_OBJMAP_SPRITE_OBJECT_HPP

#include <ClanLib/Display/sprite.h>
#include "objmap_object.hpp"

#include "meta_data.hpp"

class ObjMapSpriteObjectImpl;

/** */
class ObjMapSpriteObject
{
public:
  ObjMapSpriteObject();
  ObjMapSpriteObject(const CL_Sprite& s,
                     const CL_Pointf& pos_, 
                     const MetaData& data_);

  void set_rotate(float angle);
  void flip_horizontal();
  void flip_vertical();
  void set_sprite(const CL_Sprite& s);
  
  ObjMapObject to_object();
private:
  SharedPtr<ObjMapSpriteObjectImpl> impl;
};

#endif

/* EOF */
