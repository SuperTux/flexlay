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

class ObjMapObject;
class ObjectLayerImpl;

/** The ObjectLayer provides a simple Layer for holding positioned
    objects. Objects consist of a CL_Sprite and some properties
    accessible from scripting languages */
class ObjectLayer
{
public:
  typedef ObjMapObject Obj;
  typedef std::vector<ObjMapObject*> Objs;

  static ObjectLayer* current_;

  static ObjectLayer* current() { return current_; }
  static void set_current(ObjectLayer* c) { current_ = c; }

  ObjectLayer();
  ~ObjectLayer();

  void draw(EditorMapComponent* parent);

  /** Add an object to the map and return a handle to it */
  int  add_object(const CL_Sprite& sprite, const CL_Point& pos, const MetaData& data);

  void add_object(ObjMapObject* obj);
  void delete_object(int id);
  int  duplicate_object(int id);

  ObjMapObject* find_object(const CL_Point& pos);
  std::vector<ObjMapObject*> get_selection(const CL_Rect& rect);
  Objs* get_objects();
  ObjectLayer::Obj* get_object(int id);
  int get_next_object_handle();

  Layer to_layer();

private:
  CL_SharedPtr<ObjectLayerImpl> impl;
};

#endif

/* EOF */
