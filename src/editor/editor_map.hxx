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

class TileMapTool;

/** */
class EditorMap : public CL_Component
{
private:
  CL_SlotContainer slots;

  typedef std::vector<EditorMapLayer*> Layers;
  Layers layers;

  EditorTileMap* tilemap;
  EditorObjMap*  objmap;

  int zoom_factor;
  CL_Pointf trans_offset;
  CL_Pointf old_trans_offset;

  CL_Point click_pos;

  bool scrolling;
  typedef std::vector<TileMapTool*> Tools;
  Tools tools;
  TileMapTool* tool;

  Field<int>* diamond_map;
  std::vector<std::string> scripts;

  void cleanup();
public:
  EditorMap(const CL_Rect& rect, CL_Component* parent);
  ~EditorMap();

  void set_tool(int i);

  float get_zoom();
  void  zoom_out();
  void  zoom_in();

  void move_to(int x, int y);

  void update(float delta);
  void draw();

  void mouse_up  (const CL_InputEvent& event);
  void mouse_down(const CL_InputEvent& event);
  void mouse_move(const CL_InputEvent& event);

  TileMapTool*    get_tool_by_name(int i);
  EditorMapLayer* get_layer_by_name(int i);
  EditorMapLayer* get_layer(int i);
  void set_active_layer(int i);

  CL_Rect get_clip_rect();
  
  std::vector<std::string> get_scripts() { return scripts; }

  CL_Point screen2tile(const CL_Point& pos);
  CL_Point screen2world(const CL_Point& pos);
};

#endif

/* EOF */
