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

#ifndef HEADER_TILEMAP_LAYER_HXX
#define HEADER_TILEMAP_LAYER_HXX

#include <ClanLib/Display/pixel_buffer.h>
#include "field.hxx"
#include "shared_ptr.hxx"
#include "layer.hxx"

class Tileset;
class TileBrush;
class TilemapLayerImpl;
class EditorMapComponent;

/** */
class TilemapLayer
{
private:
  static TilemapLayer current_;
public:
  static TilemapLayer current();
  static void set_current(TilemapLayer t);

  TilemapLayer(); 
  TilemapLayer(Tileset tileset, int w,  int h);
  ~TilemapLayer();

  void draw(EditorMapComponent* parent, CL_GraphicContext* gc);

  Tileset get_tileset();

  int  get_tile (int, int);

  Field<int>* get_field();

  /** @param pos position of the old map in the new resized one
      @param size height of the new map */
 void resize(const CL_Size& size, const CL_Point& point);

  std::vector<int> get_data();
  void set_data(std::vector<int> d);

  /** Draw the gives brush to the map */
  void draw_tile(const TileBrush& brush, const CL_Point& pos);

  /** Draw the given single tile to the map */
  void draw_tile(int id, const CL_Point& pos);

  /** FIXME: Very bad naming */
  void draw_tile(int id, int x, int y, bool attribute, CL_GraphicContext* gc);

  int get_width()  const;
  int get_height() const;

  void set_background_color(const CL_Color& color);
  void set_foreground_color(const CL_Color& color);

  void set_draw_attribute(bool t);
  bool get_draw_attribute() const;

  void set_draw_grid(bool t);
  bool get_draw_grid() const;

  CL_PixelBuffer create_pixelbuffer();

  static void draw_tiles(Field<int>* field, const TileBrush& brush, const CL_Point& pos);

  bool has_bounding_rect() const;
  CL_Rect get_bounding_rect();

  /** Convert a coordinate given in world position into a tile
      coordinate */
  CL_Point world2tile(const CL_Pointf& pos) const;

  bool is_null() const { return !impl.get(); }

  Layer to_layer();

private:
  SharedPtr<TilemapLayerImpl> impl;
};

#endif

/* EOF */
