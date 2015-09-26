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

#ifndef HEADER_FLEXLAY_OBJECT_LAYER_HPP
#define HEADER_FLEXLAY_OBJECT_LAYER_HPP

#include "meta_data.hpp"
#include "layer.hpp"
#include "objmap_object.hpp"
#include "objmap_control_point.hpp"

class ObjectLayerImpl;

/** The ObjectLayer provides a simple Layer for holding positioned
    objects. Objects can consist of a Sprite and some properties
    accessible from scripting languages or any other thing that is a
    ObjMapObject. */
class ObjectLayer
{
public:
  typedef std::vector<ObjMapObject> Objects;
  typedef std::vector<ObjMapControlPoint> ControlPoints;

  static ObjectLayer current_;

  static ObjectLayer current() { return current_; }
  static void set_current(ObjectLayer c) { current_ = c; }

  ObjectLayer();
  ~ObjectLayer();

  void add_object(const ObjMapObject& obj);
  void delete_object(const ObjMapObject& obj);

  /** Moved the given object one position up in the object stack */
  void raise(const ObjMapObject& obj);

  /** Moved the given object one position down in the object stack */
  void lower(const ObjMapObject& obj);

  /** Moves the object to the given height in the object stack (0 is
      lowest position, size()-1 is highest */
  void move_to(const ObjMapObject& obj, int height);

  /** Returns the index at which the given object is in the Objects
      array or -1 if the object couldn't be found */
  int get_object_index(const ObjMapObject& obj);

  void add_control_point(const ObjMapControlPoint& obj);
  void delete_control_points();

  ObjMapObject find_object(const Pointf& pos);
  ObjMapControlPoint find_control_point(const Pointf& pos);
  std::vector<ObjMapObject> get_selection(const Rectf& rect);
  std::vector<ObjMapObject> get_objects();

  Layer to_layer();

private:
  std::shared_ptr<ObjectLayerImpl> impl;
};

#endif

/* EOF */
