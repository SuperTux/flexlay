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
#include "editor_map_layer.hxx"

/** GUI Component that holds positioned objects, ie. objects which
    consist of a CL_Sprite and some properties */
class EditorObjMap : public EditorMapLayer
{
private:
  CL_SlotContainer slots;

public:
  struct Obj {
    CL_Sprite sprite;
    CL_Point  pos;
  };

  typedef std::vector<Obj*> Objs;
  Objs objects;
public:
  EditorObjMap(EditorMap* p);
  ~EditorObjMap();

  void update(float delta);
  void draw();

  EditorObjMap::Obj* find_object(const CL_Point& pos);
private:
  EditorObjMap (const EditorObjMap&);
  EditorObjMap& operator= (const EditorObjMap&);
};

#endif

/* EOF */
