//  Flexlay - A Generic 2D Game Editor
//  Copyright (C) 2002 Ingo Ruhnke <grumbel@gmx.de>
//
//  This program is free software: you can redistribute it and/or modify
//  it under the terms of the GNU General Public License as published by
//  the Free Software Foundation, either version 3 of the License, or
//  (at your option) any later version.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//  GNU General Public License for more details.
//
//  You should have received a copy of the GNU General Public License
//  along with this program.  If not, see <http://www.gnu.org/licenses/>.

#ifndef HEADER_FLEXLAY_TILEMAP_LAYER_HPP
#define HEADER_FLEXLAY_TILEMAP_LAYER_HPP

#include <ClanLib/Display/pixel_buffer.h>
#include <memory>

#include "color.hpp"
#include "field.hpp"
#include "layer.hpp"
#include "math/point.hpp"
#include "math/size.hpp"
#include "meta_data.hpp"

class TileBrush;
class TilemapLayerImpl;
class Tileset;

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

  void draw(GraphicContext& gc);

  Tileset get_tileset();

  int  get_tile (int, int);

  Field<int>* get_field();

  /** @param pos position of the old map in the new resized one
      @param size height of the new map */
  void resize(const Size& size, const Point& point);

  const std::vector<int>& get_data();
  void set_data(std::vector<int> d);

  void   set_metadata(const MetaData& obj);
  MetaData get_metadata() const;

  /** Draw the gives brush to the map */
  void draw_tile(const TileBrush& brush, const Point& pos);

  /** Draw the given single tile to the map */
  void draw_tile(int id, const Point& pos);

  int get_width()  const;
  int get_height() const;

  void set_background_color(const Color& color);
  void set_foreground_color(const Color& color);

  void set_draw_attribute(bool t);
  bool get_draw_attribute() const;

  void set_draw_grid(bool t);
  bool get_draw_grid() const;

  CL_PixelBuffer create_pixelbuffer();

  static void draw_tiles(Field<int>* field, const TileBrush& brush, const Point& pos);

  bool has_bounding_rect() const;
  Rect get_bounding_rect() const;

  /** Convert a coordinate given in world position into a tile
      coordinate */
  Point world2tile(const Pointf& pos) const;

  bool is_null() const { return !impl.get(); }

  Layer to_layer();

private:
  std::shared_ptr<TilemapLayerImpl> impl;
};

#endif

/* EOF */
