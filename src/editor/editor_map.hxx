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

#ifndef HEADER_EDITOR_MAP_HXX
#define HEADER_EDITOR_MAP_HXX

#include <vector>
#include <ClanLib/Display/sprite.h>
#include <ClanLib/GUI/component.h>
#include <ClanLib/Core/Math/point.h>
#include "../field.hxx"
#include "editor_objmap.hxx"
#include "editor_tilemap.hxx"

class EditorMapComponent;
class TileMapTool;

/** Object which represents a level, quirled together with the GUI
    stuff */
class EditorMap
{
private:
  typedef std::vector<EditorMapLayer*> Layers;
  Layers layers;
  std::vector<std::string> scripts;

public:
  EditorMap();
  ~EditorMap();

  void draw(EditorMapComponent* parent);

  void add_layer(EditorMapLayer* layer);

  EditorMapLayer* get_layer_by_name(int i);
  EditorMapLayer* get_layer(int i);
  void set_active_layer(int i);
  
  std::vector<std::string> get_scripts() { return scripts; }
};

#endif

/* EOF */
