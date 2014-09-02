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

#include "objmap_sprite_object.hpp"

#include "objmap_object_impl.hpp"

class ObjMapSpriteObjectImpl : public ObjMapObjectImpl
{
public:
  CL_Sprite sprite;

  void draw(CL_GraphicContext* gc);
  Rectf get_bound_rect() const;

  ObjMapObject*  duplicate(int handle_);
};

ObjMapSpriteObject::ObjMapSpriteObject()
{
}

ObjMapSpriteObject::ObjMapSpriteObject(const CL_Sprite& sprite_,
                                       const Pointf& pos_,
                                       const MetaData& data_)
  : impl(new ObjMapSpriteObjectImpl())
{
  impl->pos    = pos_;
  impl->data   = data_;
  impl->sprite = sprite_;
}

void
ObjMapSpriteObjectImpl::draw(CL_GraphicContext* gc)
{
  sprite.draw(pos.x, pos.y, gc);
}

Rectf
ObjMapSpriteObjectImpl::get_bound_rect() const
{
  Point  align = Point(0, 0);
  CL_Origin origin_e;

  sprite.get_alignment(origin_e, align.x, align.y);

  Point origin = calc_origin(static_cast<Origin>(origin_e),
                             Size(sprite.get_width(),
                                  sprite.get_height()));
  align.x = -align.x;

  // FIXME: This looks a bit hacky
  float scale_x, scale_y;
  sprite.get_scale(scale_x, scale_y);

  if (scale_x < 0)
    align.x += sprite.get_width();

  if (scale_y < 0)
    align.y += sprite.get_height();

  //  if (scale_x > 1.0f && scale_y > 1.0f)
  //    return Rectf(pos - origin - align,
  //                   Sizef(sprite.get_width() * scale_x, sprite.get_height() * scale_y));
  //  else
  return Rectf(pos - origin - align,
                  Sizef(sprite.get_width(), sprite.get_height()));
}

void
ObjMapSpriteObject::flip_vertical()
{
  float scale_x, scale_y;

  impl->sprite.get_scale(scale_x, scale_y);
  impl->sprite.set_scale(scale_x, -scale_y);
  if (scale_y < 0)
    impl->pos.y -= impl->sprite.get_height();
  else
    impl->pos.y += impl->sprite.get_height();
}

void
ObjMapSpriteObject::flip_horizontal()
{
  float scale_x, scale_y;
  impl->sprite.get_scale(scale_x, scale_y);
  impl->sprite.set_scale(-scale_x, scale_y);
  if (scale_x < 0)
    impl->pos.x -= impl->sprite.get_width();
  else
    impl->pos.x += impl->sprite.get_width();
}

void
ObjMapSpriteObject::set_sprite(const CL_Sprite& sprite)
{
  impl->sprite = sprite;
}

void
ObjMapSpriteObject::set_rotate(float angle)
{
  impl->sprite.set_angle(angle);
}

ObjMapObject
ObjMapSpriteObject::to_object()
{
  return ObjMapObject(std::shared_ptr<ObjMapObjectImpl>(impl));
}

/* EOF */
