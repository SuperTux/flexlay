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

#ifndef HEADER_FLEXLAY_OBJMAP_OBJECT_HPP
#define HEADER_FLEXLAY_OBJMAP_OBJECT_HPP

#include <boost/signals2.hpp>

#include "math/rect.hpp"
#include "meta_data.hpp"

class GraphicContext;
class ObjMapObjectImpl;

class ObjMapObject
{
public:
  ObjMapObject();
  ObjMapObject(const std::shared_ptr<ObjMapObjectImpl>& impl_);
  virtual ~ObjMapObject() {}

  Pointf get_pos() const;
  void     set_pos(const Pointf& p);

  MetaData get_metadata() const;
  void     set_metadata(const MetaData& data_);

  boost::signals2::signal<void (ObjMapObject)>& sig_move();
  boost::signals2::signal<void (ObjMapObject)>& sig_select();
  boost::signals2::signal<void (ObjMapObject)>& sig_deselect();

  void draw(GraphicContext& gc);
  Rectf get_bound_rect() const;

  void add_control_points();
  void update_control_points();

  bool is_null() const;

  bool operator==(const ObjMapObject& obj) const;
  bool operator<(const ObjMapObject& obj) const;

private:
  std::shared_ptr<ObjMapObjectImpl> impl;
};

#endif

/* EOF */
