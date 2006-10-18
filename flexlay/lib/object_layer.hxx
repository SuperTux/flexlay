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

#ifndef HEADER_OBJECT_LAYER_HXX
#define HEADER_OBJECT_LAYER_HXX

#include <vector>
#include <ClanLib/Display/sprite.h>
#include <ClanLib/GUI/component.h>
#include <ClanLib/Core/Math/point.h>
#include "meta_data.hxx"
#include "layer.hxx"
#include "objmap_object.hxx"
#include "objmap_control_point.hxx"
#include "shared_ptr.hxx"

class ObjectLayerImpl;

/** The ObjectLayer provides a simple Layer for holding positioned
    objects. Objects can consist of a CL_Sprite and some properties
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

  ObjMapObject find_object(const CL_Pointf& pos);
  ObjMapControlPoint find_control_point(const CL_Pointf& pos);
  std::vector<ObjMapObject> get_selection(const CL_Rectf& rect);
  std::vector<ObjMapObject> get_objects();

  Layer to_layer();

private:
  SharedPtr<ObjectLayerImpl> impl;
};

#endif

/* EOF */
