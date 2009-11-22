//  $Id$
// 
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

#ifndef HEADER_OBJECT_BRUSH_HXX
#define HEADER_OBJECT_BRUSH_HXX

#include <ClanLib/Display/sprite.h>
#include <ClanLib/Core/Math/point.h>
#include "object_layer.hpp"
#include "objmap_object.hpp"
#include "objmap_sprite_object.hpp"
#include "meta_data.hpp"

class ObjectBrushImpl;

class ObjectBrush
{
public:
  ObjectBrush();
  ObjectBrush(const CL_Sprite& sprite_,
              const MetaData& data_);

  CL_Sprite get_sprite();
  MetaData get_data();

  ObjMapSpriteObject to_sprite_object(const CL_Pointf& pos);
private:
  SharedPtr<ObjectBrushImpl> impl;
};

#endif

/* EOF */
