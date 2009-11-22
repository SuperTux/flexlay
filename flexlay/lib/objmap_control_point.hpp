//  Pingus - A free Lemmings clone
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

#ifndef HEADER_FLEXLAY_OBJMAP_CONTROL_POINT_HPP
#define HEADER_FLEXLAY_OBJMAP_CONTROL_POINT_HPP

#include <ClanLib/Display/sprite.h>

#include "meta_data.hpp"

class ObjMapControlPointImpl;

/** An ObjMapControlPoint is used to control a property of an object,
    such as size, rotation or scaling. ControlPoints get drawn around
    the object in a size which is independend of the current zoom
    level and can be draged around with the mous. */
class ObjMapControlPoint
{
private:
public:
  ObjMapControlPoint() : impl() {}
  ObjMapControlPoint(CL_Sprite sprite_, CL_Pointf pos_, MetaData data_);

  CL_Pointf get_pos() const;
  void     set_pos(const CL_Pointf& p);
  void     set_pos_raw(const CL_Pointf& p);
  void     draw(CL_GraphicContext* gc);

  CL_Rect get_bound_rect() const;

  CL_Signal_v1<CL_Pointf>& sig_set_pos();

  bool is_null() const { return !impl.get(); }

private:
  boost::shared_ptr<ObjMapControlPointImpl> impl;
};

#endif

/* EOF */
