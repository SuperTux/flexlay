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

#ifndef HEADER_OBJMAP_RECT_OBJECT_HXX
#define HEADER_OBJMAP_RECT_OBJECT_HXX

#include <ClanLib/Core/Math/rect.h>
#include <ClanLib/Display/color.h>
#include "objmap_object.hxx"
#include "objmap_object_impl.hxx"

class ObjMapRectObjectImpl;

class ObjMapRectObject
{
public:
  ObjMapRectObject(const CL_Rect&  rect_,
                   const CL_Color& color_,
                   const MetaData& data_);
    
  void draw(CL_GraphicContext* gc);

  void set_color(const CL_Color& color);

  void set_rect(const CL_Rect& rect);
  CL_Rectf get_rect() const;

  ObjMapObject to_object();
private:
  SharedPtr<ObjMapRectObjectImpl> impl;
};

#endif

/* EOF */
