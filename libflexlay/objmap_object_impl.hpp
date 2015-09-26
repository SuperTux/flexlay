// Flexlay - A Generic 2D Game Editor
// Copyright (C) 2002 Ingo Ruhnke <grumbel@gmail.com>
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <http://www.gnu.org/licenses/>.

#ifndef HEADER_FLEXLAY_OBJMAP_OBJECT_IMPL_HPP
#define HEADER_FLEXLAY_OBJMAP_OBJECT_IMPL_HPP

#include "objmap_object.hpp"

class ObjMapObjectImpl
{
public:
  Pointf  pos;
  MetaData  data;

  boost::signals2::signal<void (ObjMapObject)> on_move;
  boost::signals2::signal<void (ObjMapObject)> on_select;
  boost::signals2::signal<void (ObjMapObject)> on_deselect;

  ObjMapObjectImpl();
  virtual ~ObjMapObjectImpl();

  virtual void draw(GraphicContext& gc) =0;
  virtual Rectf get_bound_rect() const  =0;

  virtual void add_control_points();
  virtual void update_control_points();
};

#endif

/* EOF */
