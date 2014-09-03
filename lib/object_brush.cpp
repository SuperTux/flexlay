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

#include "object_brush.hpp"

class ObjectBrushImpl
{
public:
  Sprite sprite;
  MetaData  data;
};

ObjectBrush::ObjectBrush()
{

}

ObjectBrush::ObjectBrush(const Sprite& sprite_,
                         const MetaData& data_)
  : impl(new ObjectBrushImpl())
{
  impl->sprite = sprite_;
  impl->data   = data_;
}

Sprite
ObjectBrush::get_sprite()
{
  return impl->sprite;
}

MetaData
ObjectBrush::get_data()
{
  return impl->data;
}

ObjMapSpriteObject
ObjectBrush::to_sprite_object(const Pointf& pos)
{
  ObjMapSpriteObject obj(impl->sprite, pos, impl->data);
  return obj;
}

/* EOF */
