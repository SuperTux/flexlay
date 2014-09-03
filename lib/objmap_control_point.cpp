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

#include "objmap_control_point.hpp"

#include "graphic_context.hpp"

class ObjMapControlPointImpl
{
public:
  Sprite sprite;
  Pointf  pos;
  MetaData  data;

  void draw(GraphicContext& gc);
  Rect get_bound_rect() const;
  boost::signals2::signal<void (Pointf)> on_set_pos;
};

boost::signals2::signal<void (Pointf)>&
ObjMapControlPoint::sig_set_pos()
{
  return impl->on_set_pos;
}

ObjMapControlPoint::ObjMapControlPoint(Sprite sprite_, Pointf pos_, MetaData data_)
  : impl(new ObjMapControlPointImpl)
{
  impl->sprite = sprite_;
  impl->pos    = pos_;
  impl->data   = data_;
}

void
ObjMapControlPoint::draw(GraphicContext& gc)
{
  impl->draw(gc);
}

void
ObjMapControlPointImpl::draw(GraphicContext& gc)
{
  sprite.draw(static_cast<int>(pos.x), static_cast<int>(pos.y), gc.gc);
}

void
ObjMapControlPoint::set_pos_raw(const Pointf& p)
{
  impl->pos = p;
}

void
ObjMapControlPoint::set_pos(const Pointf& p)
{
  impl->on_set_pos(p);
}

Pointf
ObjMapControlPoint::get_pos() const
{
  return impl->pos;
}

Rect
ObjMapControlPoint::get_bound_rect() const
{
  return impl->get_bound_rect();
}

Rect
ObjMapControlPointImpl::get_bound_rect() const
{
  Point  align = Point(0, 0);
  CL_Origin origin_e;

  sprite.get_alignment(origin_e, align.x, align.y);

  Point origin = calc_origin(static_cast<Origin>(origin_e),
                             Size(sprite.get_width(),
                                  sprite.get_height()));
  align.x = -align.x;

  return Rect(Point(pos) - origin - align,
              Size(sprite.get_width(), sprite.get_height()));
}

/* EOF */
