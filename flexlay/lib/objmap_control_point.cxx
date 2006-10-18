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
  CL_Pointf  pos;
  MetaData  data;

  void draw(CL_GraphicContext* gc);
  CL_Rect get_bound_rect() const;
  CL_Signal_v1<CL_Pointf> on_set_pos;
};

CL_Signal_v1<CL_Pointf>& 
ObjMapControlPoint::sig_set_pos()
{
  return impl->on_set_pos;
}

ObjMapControlPoint::ObjMapControlPoint(CL_Sprite sprite_, CL_Pointf pos_, MetaData data_)
  : impl(new ObjMapControlPointImpl)
{
  impl->sprite = sprite_;
  impl->pos    = pos_;
  impl->data   = data_;
}

void
ObjMapControlPoint::draw(CL_GraphicContext* gc)
{
  impl->draw(gc);
}

void
ObjMapControlPointImpl::draw(CL_GraphicContext* gc)
{
  sprite.draw(static_cast<int>(pos.x), static_cast<int>(pos.y), gc);
}

void
ObjMapControlPoint::set_pos_raw(const CL_Pointf& p)
{
  impl->pos = p;
}

void
ObjMapControlPoint::set_pos(const CL_Pointf& p)
{
  impl->on_set_pos(p);
}

CL_Pointf
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
      
  return CL_Rect(CL_Point(pos) - origin - align,
                 CL_Size(sprite.get_width(), sprite.get_height()));
}

/* EOF */
