//  $Id: editor_tilemap.hxx,v 1.10 2003/09/26 14:29:36 grumbel Exp $
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
#include "editor_map_layer.hxx"
#include "editor_tile.hxx"

typedef Field<int> TileBrush;

/** Holds the tilemap data for the editor and provides functions to
    manipulate them */
class EditorTileMap : public EditorMapLayer
{
private:
  typedef std::vector<Field<EditorTile*>*> Fields;
  typedef Field<EditorTile*>::iterator FieldIter;

  Fields fields;
  Field<EditorTile*>* current_field;

  bool scrolling;

  Field<int>* diamond_map;

  void cleanup();
public:
  int brush_tile;

  EditorTileMap();
  ~EditorTileMap();

  void draw ();
  void draw_map(Field<EditorTile*>* field);

  void update(float delta) {}

  /** Return the current active field */
  Field<EditorTile*>* get_field() { return current_field; }
  Field<int>*         get_diamond_map() { return diamond_map; }

  EditorTile* get_tile (int, int);
  void set_active_layer(int i);

  void load (const std::string& filename);
  void new_level(int w, int h);

  void resize(int w, int h);

  Field<EditorTile*>* get_map(int i);

  /** Draw the gives brush to the map 
   *  @param opaque if true draw fully transparent tiles the same as
   *  @param opaque tiles, else don't draw transparent tiles at all to
   *  @param opaque the map
   */
  void draw_tile(const TileBrush& brush, const CL_Point& pos, bool opaque);

  /** Draw the given single tile to the map */
  void draw_tile(int id, const CL_Point& pos);

  int get_width()  { return current_field->get_width(); }
  int get_height() { return current_field->get_height(); }
};

#endif

/* EOF */
