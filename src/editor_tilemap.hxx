//  $Id: editor_tilemap.hxx,v 1.10 2003/09/26 14:29:36 grumbel Exp $
// 
//  Flexlay - A Generic 2D Game Editor
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
#include <ClanLib/Display/color.h>
#include "field.hxx"
#include "editor_map_layer.hxx"

class Tileset;
class CL_PixelBuffer;
class TileBrush;

/** EditorTileMap holds the tilemap data for the editor and provides
    functions to manipulate them. Each \a EditorTilemap is associated
    with a \a Tileset, which provides information on which ids are
    mapped to which Tiles, the tilemap itself only knows the ids of
    tiles.  */
class EditorTileMap : public EditorMapLayer
{
private:
  Tileset* tileset;
  CL_Color background_color;
  CL_Color foreground_color;
  bool hex_mode;

  Field<int> field;

  bool draw_grid;
  bool draw_attribute;

  static EditorTileMap* current_;
public:
  static EditorTileMap* current() { return current_; }
  static void set_current(EditorTileMap* c) { current_ = c; }
  
  EditorTileMap(Tileset* tileset, int w,  int h);
  ~EditorTileMap();

  void draw (EditorMapComponent* parent);

  /** Return a pointer to the raw field representing this map */
  Field<int>* get_field() { return &field; }

  Tileset* get_tileset() { return tileset; }

  int  get_tile (int, int);

  /** @param x position of the old map in the new resized one
      @param y position of the old map in the new resized one
      @param w height of the new map
      @param h height of the new map */
  void resize(const CL_Size& size, const CL_Point& point);

  Field<int>* get_map() { return &field; }

  std::vector<int> get_data() { return field.get_data(); }
  void set_data(std::vector<int> d) { field.set_data(d); }

  /** Draw the gives brush to the map */
  void draw_tile(const TileBrush& brush, const CL_Point& pos);

  /** Draw the given single tile to the map */
  void draw_tile(int id, const CL_Point& pos);

  void draw_tile(int id, int x, int y, bool attribute);

  int get_width()  const { return field.get_width(); }
  int get_height() const { return field.get_height(); }

  void set_background_color(const CL_Color& color) { background_color = color; }
  void set_foreground_color(const CL_Color& color) { foreground_color = color; }

  void set_draw_attribute(bool t);
  bool get_draw_attribute() const;

  void set_draw_grid(bool t);
  bool get_draw_grid() const;

  CL_PixelBuffer create_pixelbuffer();

  static void draw_tile(Field<int>* field, const TileBrush& brush, const CL_Point& pos);

  bool has_bounding_rect() const { return true; }
  CL_Rect get_bounding_rect();

  /** Convert a coordinate given in world position into a tile
      coordinate */
  CL_Point world2tile(const CL_Point& pos) const;
};

#endif

/* EOF */
