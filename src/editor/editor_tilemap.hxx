//  $Id: editor_tilemap.hxx,v 1.7 2003/09/23 19:10:05 grumbel Exp $
// 
//  Pingus - A free Lemmings clone
//  Copyright (C) 2000 Ingo Ruhnke <grumbel@gmx.de>
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

#ifndef EDITORTILEMAP_HXX
#define EDITORTILEMAP_HXX

#include <ClanLib/gui.h>
#include "../field.hxx"
#include "editor_tile.hxx"

class TileMapTool;

/** Holds the tilemap data for the editor and provides functions to
    manipulate them */
class EditorTileMap : public CL_Component
{
private:
  CL_SlotContainer slots;
  typedef std::vector<Field<EditorTile*>*> Fields;
  Fields fields;
  Field<EditorTile*>* current_field;
  typedef Field<EditorTile*>::iterator FieldIter;
  
  int zoom_factor;
  CL_Pointf trans_offset;
  CL_Pointf old_trans_offset;

  CL_Point click_pos;

  bool scrolling;
  TileMapTool* tool;

  void cleanup();
public:
  int brush_tile;

  EditorTileMap (CL_Component* parent);
  ~EditorTileMap();

  void set_tool(int i);

  float get_zoom();
  void zoom_out();
  void zoom_in();

  void draw ();
  void draw_map(Field<EditorTile*>* field);

  void mouse_up(const CL_InputEvent& event);
  void mouse_down(const CL_InputEvent& event);
  void mouse_move(const CL_InputEvent& event);

  /** Return the current active field */
  Field<EditorTile*>* get_field() { return current_field; }

  EditorTile* get_tile (int, int);
  void set_active_layer(int i);

  void load (const std::string& filename);
  void save (const std::string& filename);
  void new_level(int w, int h);

  CL_Rect get_clip_rect();
  
  Field<EditorTile*>* get_map(int i);
  int get_width()  { return current_field->get_width(); }
  int get_height() { return current_field->get_height(); }

  CL_Point screen2tile(const CL_Point& pos);
};

#endif

/* EOF */
