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

#ifndef HEADER_EDITOR_OBJMAP_HXX
#define HEADER_EDITOR_OBJMAP_HXX

#include <vector>
#include <ClanLib/Display/sprite.h>
#include <ClanLib/GUI/component.h>
#include <ClanLib/Core/Math/point.h>
#include "../scm_obj.hxx"
#include "editor_map_layer.hxx"

/** GUI Component that holds positioned objects, ie. objects which
    consist of a CL_Sprite and some properties */
class EditorObjMap : public EditorMapLayer
{
private:
  CL_SlotContainer slots;

public:
  struct Obj {
    int       handle;
    CL_Sprite sprite;
    CL_Point  pos;
    SCMObj    data;
  };

  typedef std::vector<Obj*> Objs;
  Objs objects;

  int handle_count;
public:
  EditorObjMap();
  ~EditorObjMap();

  void update(float delta);
  void draw(EditorMapComponent* parent);

  /** Add an object to the map and return a handle to it */
  int add_object(const CL_Sprite& sprite, const CL_Point& pos, const SCMObj& data);

  CL_Rect get_bounding_rect(const CL_Sprite& sprite);

  EditorObjMap::Obj* find_object(const CL_Point& pos);
  std::vector<EditorObjMap::Obj*> get_selection(const CL_Rect& rect);
  Objs* get_objects();
  EditorObjMap::Obj* get_object(int id);
private:
  EditorObjMap (const EditorObjMap&);
  EditorObjMap& operator= (const EditorObjMap&);
};

#endif

/* EOF */
