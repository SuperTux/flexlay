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
  int tile_size;

  Field<int> field;

  bool draw_grid;
  bool draw_attribute;

public:
  EditorTileMap(int w,  int h, int tile_size_);
  ~EditorTileMap();

  void draw (EditorMapComponent* parent);

  /** Return a pointer to the raw field representing this map */
  Field<int>* get_field() { return &field; }

  int  get_tile (int, int);

  /** @param x position of the old map in the new resized one
      @param y position of the old map in the new resized one
      @param w height of the new map
      @param h height of the new map */
  void resize(int w, int h, int x, int y);

  Field<int>* get_map() { return &field; }

  /** Draw the gives brush to the map */
  void draw_tile(const TileBrush& brush, const CL_Point& pos);

  /** Draw the given single tile to the map */
  void draw_tile(int id, const CL_Point& pos);

  void draw_tile(int id, int x, int y, bool attribute, float alpha);

  int get_width()  const { return field.get_width(); }
  int get_height() const { return field.get_height(); }

  void set_draw_attribute(bool t);
  bool get_draw_attribute() const;

  void set_draw_grid(bool t);
  bool get_draw_grid() const;

  static void draw_tile(Field<int>* field, const TileBrush& brush, const CL_Point& pos);
};

#endif

/* EOF */
