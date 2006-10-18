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

#ifndef HEADER_OBJMAP_OBJECT_IMPL_HXX
#define HEADER_OBJMAP_OBJECT_IMPL_HXX

#include <ClanLib/Core/Math/point.h>
#include <ClanLib/Core/Math/rect.h>
#include "objmap_object.hxx"
#include "meta_data.hxx"

class ObjMapObjectImpl 
{
public:
  CL_Pointf  pos;
  MetaData  data;

  CL_Signal_v1<ObjMapObject> on_move;
  CL_Signal_v1<ObjMapObject> on_select;
  CL_Signal_v1<ObjMapObject> on_deselect;

  ObjMapObjectImpl();
  virtual ~ObjMapObjectImpl();

  virtual void draw(CL_GraphicContext* gc) =0;
  virtual CL_Rectf get_bound_rect() const  =0;

  virtual void add_control_points();
  virtual void update_control_points();
};

#endif

/* EOF */
