//  $Id$
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

#include "objmap_control_point.hxx"

class ObjMapControlPointImpl
{
public:
  CL_Sprite sprite;
  CL_Point  pos;
  MetaData  data;

  void draw();
  CL_Rect get_bound_rect() const;
};

ObjMapControlPoint::ObjMapControlPoint(CL_Sprite sprite_, CL_Point pos_, MetaData data_)
  : impl(new ObjMapControlPointImpl)
{
  impl->sprite = sprite_;
  impl->pos    = pos_;
  impl->data   = data_;
}

void
ObjMapControlPoint::draw()
{
  impl->draw();
}

void
ObjMapControlPointImpl::draw()
{
  sprite.draw(pos.x, pos.y);
}

void
ObjMapControlPoint::set_pos(const CL_Point& p)
{
  impl->pos = p;
}

CL_Point
ObjMapControlPoint::get_pos() const
{
  return impl->pos;
}

CL_Rect
ObjMapControlPoint::get_bound_rect() const
{
  return impl->get_bound_rect();
}

CL_Rect
ObjMapControlPointImpl::get_bound_rect() const
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

/* EOF */
