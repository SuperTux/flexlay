//  $Id$
// 
//  Flexlay - A Generic 2D Game Editor
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

#ifndef HEADER_OBJMAP_SPRITE_OBJECT_HXX
#define HEADER_OBJMAP_SPRITE_OBJECT_HXX

#include <ClanLib/Display/sprite.h>
#include <ClanLib/Core/Math/point.h>
#include "shared_ptr.hxx"
#include "meta_data.hxx"
#include "objmap_object.hxx"

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
  
  ObjMapObject to_object();
private:
  SharedPtr<ObjMapSpriteObjectImpl> impl;
};

#endif

/* EOF */
