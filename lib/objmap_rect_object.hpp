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

#ifndef HEADER_FLEXLAY_OBJMAP_RECT_OBJECT_HPP
#define HEADER_FLEXLAY_OBJMAP_RECT_OBJECT_HPP

#include <ClanLib/Core/Math/rect.h>
#include <ClanLib/Display/color.h>
#include "objmap_object.hpp"
#include "objmap_object_impl.hpp"

class ObjMapRectObjectImpl;

class ObjMapRectObject
{
public:
  ObjMapRectObject(const CL_Rect&  rect_,
                   const CL_Color& color_,
                   const MetaData& data_);

  void set_color(const CL_Color& color);

  void set_rect(const CL_Rect& rect);
  CL_Rectf get_rect() const;

  ObjMapObject to_object();
private:
  std::shared_ptr<ObjMapRectObjectImpl> impl;
};

#endif

/* EOF */
