//  $Id$
// 
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

#ifndef HEADER_OBJMAP_OBJECT_HXX
#define HEADER_OBJMAP_OBJECT_HXX

#include <ClanLib/signals.h>
#include <ClanLib/Display/graphic_context.h>
#include <ClanLib/Core/Math/point.h>
#include <ClanLib/Core/Math/rect.h>
#include "meta_data.hpp"

class ObjMapObjectImpl;

/** */
class ObjMapObject
{
public:
  ObjMapObject();
  ObjMapObject(const SharedPtr<ObjMapObjectImpl>& impl_);
  virtual ~ObjMapObject() {}

  CL_Pointf get_pos() const;
  void     set_pos(const CL_Pointf& p);

  MetaData get_metadata() const;
  void     set_metadata(const MetaData& data_);

  CL_Signal_v1<ObjMapObject>& sig_move();
  CL_Signal_v1<ObjMapObject>& sig_select();
  CL_Signal_v1<ObjMapObject>& sig_deselect();

  void draw(CL_GraphicContext* gc);
  CL_Rectf get_bound_rect() const;

  void add_control_points();
  void update_control_points();

  bool is_null() const;

  bool operator==(const ObjMapObject& obj) const;
  bool operator<(const ObjMapObject& obj) const;
private:
  SharedPtr<ObjMapObjectImpl> impl;
};

#endif

/* EOF */
