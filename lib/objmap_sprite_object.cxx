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

#include <ClanLib/Display/display.h>
#include "objmap_object_impl.hxx"
#include "objmap_sprite_object.hxx"

class ObjMapSpriteObjectImpl : public ObjMapObjectImpl
{
public:
  CL_Sprite sprite;

  void draw(CL_GraphicContext* gc);
  CL_Rectf get_bound_rect() const;

  ObjMapObject*  duplicate(int handle_);
};

ObjMapSpriteObject::ObjMapSpriteObject()
{
}

ObjMapSpriteObject::ObjMapSpriteObject(const CL_Sprite& sprite_,
                                       const CL_Pointf& pos_, 
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

CL_Rectf
ObjMapSpriteObjectImpl::get_bound_rect() const
{
  CL_Point  align = CL_Point(0, 0);
  CL_Origin origin_e;
  
  sprite.get_alignment(origin_e, align.x, align.y);

  CL_Point origin = calc_origin(origin_e, CL_Size(sprite.get_width(),
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
  //    return CL_Rectf(pos - origin - align,
  //                   CL_Sizef(sprite.get_width() * scale_x, sprite.get_height() * scale_y));
//  else
  return CL_Rectf(pos - origin - align,
                  CL_Sizef(sprite.get_width(), sprite.get_height()));  
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
ObjMapSpriteObject::set_rotate(float angle)
{
  impl->sprite.set_angle(angle);
}

ObjMapObject
ObjMapSpriteObject::to_object()
{
  return ObjMapObject(SharedPtr<ObjMapObjectImpl>(impl));
}

/* EOF */
