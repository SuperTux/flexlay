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

class TileBrush;

/** Holds the tilemap data for the editor and provides functions to
    manipulate them */
class EditorTileMap : public EditorMapLayer
{
private:
  typedef std::vector<Field<int>*> Fields;
  typedef Field<int>::iterator FieldIter;

  Fields fields;
  Field<int>* current_field;

  bool draw_grid;

  Field<int>* diamond_map;

  void cleanup();

  static EditorTileMap* current_;
public:
  static EditorTileMap* current() { return current_; } 
  
  EditorTileMap();
  ~EditorTileMap();

  void draw (EditorMapComponent* parent);
  void draw_map(EditorMapComponent* parent, Field<int>* field);

  void update(float delta) {}

  /** Return the current active field */
  Field<int>* get_field() { return current_field; }
  Field<int>* get_diamond_map() { return diamond_map; }

  int  get_tile (int, int);
  void set_active_layer(int i);

  void load (const std::string& filename);
  void new_level(int w, int h);

  /** @param x position of the old map in the new resized one
      @param y position of the old map in the new resized one
      @param w height of the new map
      @param h height of the new map */
  void resize(int w, int h, int x, int y);

  Field<int>* get_map(int i);

  /** Draw the gives brush to the map */
  void draw_tile(const TileBrush& brush, const CL_Point& pos);

  /** Draw the given single tile to the map */
  void draw_tile(int id, const CL_Point& pos);

  int get_width()  { return current_field->get_width(); }
  int get_height() { return current_field->get_height(); }

  void set_draw_grid(bool t);
  bool get_draw_grid() const;

  static void draw_tile(Field<int>* field, const TileBrush& brush, const CL_Point& pos);
};

#endif

/* EOF */
