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
#include "objmap_sprite_object.hxx"

ObjMapSpriteObject::ObjMapSpriteObject(int handle_, const CL_Point& pos_, const SCMObj& data_, 
                                       const CL_Sprite& sprite_)
  : ObjMapObject(handle_, pos_, data_), sprite(sprite_)
{
  
}

ObjMapSpriteObject::ObjMapSpriteObject(int handle_, const ObjMapSpriteObject& obj)
  : ObjMapObject(handle_, obj), sprite(obj.sprite)
{
}

void
ObjMapSpriteObject::draw()
{
  sprite.draw(pos.x, pos.y);
}

CL_Rect
ObjMapSpriteObject::get_bound_rect() const
{
  CL_Point  align = CL_Point(0, 0);
  CL_Origin origin_e;

  sprite.get_alignment(origin_e, align.x, align.y);

  CL_Point origin = calc_origin(origin_e, CL_Size(sprite.get_width(),
                                                  sprite.get_height()));
  align.x = -align.x;
      
  return CL_Rect(pos - origin - align,
                 CL_Size(sprite.get_width(), sprite.get_height()));
}

void
ObjMapSpriteObject::flip_vertical()
{
  float scale_x, scale_y;
  sprite.get_scale(scale_x, scale_y);
  sprite.set_scale(scale_x, -scale_y);
}

void
ObjMapSpriteObject::flip_horizontal()
{
  float scale_x, scale_y;
  sprite.get_scale(scale_x, scale_y);
  sprite.set_scale(-scale_x, scale_y);
}

ObjMapObject*
ObjMapSpriteObject::duplicate(int handle_)
{
  return new ObjMapSpriteObject(handle_, *this);
}

/* EOF */
