// Flexlay - A Generic 2D Game Editor
// Copyright (C) 2002 Ingo Ruhnke <grumbel@gmail.com>
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <http://www.gnu.org/licenses/>.

#include "tilemap_layer.hpp"

#include "blitter.hpp"
#include "display.hpp"
#include "graphic_context.hpp"
#include "gui/editor_map_component.hpp"
#include "layer_impl.hpp"
#include "sprite.hpp"
#include "tile.hpp"
#include "tile_brush.hpp"
#include "tileset.hpp"

TilemapLayer TilemapLayer::current_;

class TilemapLayerImpl : public LayerImpl
{
public:
  Tileset tileset;
  Color background_color;
  Color foreground_color;

  Field<int> field;

  bool draw_grid;
  bool draw_attribute;

  MetaData metadata;

  TilemapLayerImpl() {}
  virtual ~TilemapLayerImpl() {}

  bool has_bounding_rect() const override;
  Rect get_bounding_rect() const override;

  void draw(GraphicContext& gc) override;
};

TilemapLayer::TilemapLayer()
{
}

/*TilemapLayer::TilemapLayer(const std::shared_ptr<TilemapLayerImpl>& i)
  : impl(i)
  {
  }*/

TilemapLayer::TilemapLayer(Tileset tileset_, int w,  int h)
  : impl(new TilemapLayerImpl())
{
  impl->field = Field<int>(w, h);

  // FIXME: Move this to the widget or to some more generic
  // map-properties thingy
  impl->draw_grid      = false;
  impl->draw_attribute = false;

  for (int y = 0; y < impl->field.get_height(); ++y)
    for (int x = 0; x < impl->field.get_width(); ++x)
      impl->field.at(x, y) = 0;

  impl->background_color = Color(0, 0, 0, 0);
  impl->foreground_color = Color(255, 255, 255, 255);

  impl->tileset = tileset_;
}

TilemapLayer::~TilemapLayer()
{
}

void
TilemapLayer::draw(GraphicContext& gc)
{
  impl->draw(gc);
}

void
TilemapLayerImpl::draw(GraphicContext& gc)
{
  int tile_size = this->tileset.get_tile_size();

  if (false && this->background_color.get_alpha() != 0)
  {
    gc.fill_rect(Rect(Point(0,0),
                      Size(this->field.get_width()  * tile_size,
                           this->field.get_height() * tile_size)),
                 this->background_color);
  }

  Rect rect(gc.get_clip_rect());

  int start_x = std::max(0, rect.left / tile_size);
  int start_y = std::max(0, rect.top  / tile_size);
  int end_x   = std::min(this->field.get_width(),  rect.right  / tile_size + 1);
  int end_y   = std::min(this->field.get_height(), rect.bottom / tile_size + 1);

  if (foreground_color != Color(255, 255, 255, 255))
  {
    for (int y = start_y; y < end_y; ++y)
      for (int x = start_x; x < end_x; ++x)
      {
        int tile_id = this->field.at(x, y);
        if (tile_id)
        {
          Tile* tile = tileset.create(tile_id);
          if (tile) // skip transparent tile for faster draw
          {
            Sprite sprite = tile->get_sprite();
            sprite.set_color(foreground_color);
            sprite.draw(x * tile_size, y * tile_size, gc);

            if (draw_attribute)
              gc.fill_rect(Rect(Point(x, y), Size(tileset.get_tile_size(),
                                                  tileset.get_tile_size())),
                           tile->get_attribute_color());
          }
        }
      }
  }
  else
  {
    for (int y = start_y; y < end_y; ++y)
      for (int x = start_x; x < end_x; ++x)
      {
        int tile_id = this->field.at(x, y);
        if (tile_id) // skip transparent tile for faster draw
        {
          Tile* tile = tileset.create(this->field.at(x, y));
          if (tile)
          {
            tile->get_sprite().draw(x * tile_size, y * tile_size, gc);

            if (draw_attribute)
              gc.fill_rect(Rect(Point(x, y), Size(tileset.get_tile_size(),
                                                  tileset.get_tile_size())),
                           tile->get_attribute_color());
          }
        }
      }
  }

  if (this->draw_grid)
  {
    for (int y = start_y; y <= end_y; ++y)
      gc.draw_line(start_x * tile_size,
                   y       * tile_size,
                   end_x   * tile_size,
                   y       * tile_size,
                   y % 2 ? Color(150, 150, 150) : Color(255, 255, 255));

    for (int x = start_x; x <= end_x; ++x)
      gc.draw_line(x       * tile_size,
                   start_y * tile_size,
                   x       * tile_size,
                   end_y   * tile_size,
                   x % 2 ? Color(150, 150, 150) : Color(255, 255, 255));
  }

  gc.flush();
}

int
TilemapLayer::get_tile (int x, int y)
{
  if (x >= 0 && x < (int)impl->field.get_width() &&
      y >= 0 && y < (int)impl->field.get_height())
    return impl->field.at(x, y);
  else
    return 0;
}

void
TilemapLayer::resize(const Size& size, const Point& point)
{
  impl->field.resize(size.width, size.height, point.x, point.y);
}

void
TilemapLayer::draw_tile(int id, const Point& pos)
{
  if (pos.x >= 0 && pos.x < impl->field.get_width()
      && pos.y >= 0 && pos.y < impl->field.get_height())
  {
    impl->field.at(pos.x, pos.y) = id;
  }
}

void
TilemapLayer::draw_tile(const TileBrush& brush, const Point& pos)
{
  draw_tiles(&impl->field, brush, pos);
}

void
TilemapLayer::draw_tiles(Field<int>* field, const TileBrush& brush, const Point& pos)
{
  int start_x = std::max(0, -pos.x);
  int start_y = std::max(0, -pos.y);

  int end_x = std::min(brush.get_width(),  field->get_width()  - pos.x);
  int end_y = std::min(brush.get_height(), field->get_height() - pos.y);

  for (int y = start_y; y < end_y; ++y)
    for (int x = start_x; x < end_x; ++x)
    {
      if (brush.is_opaque() || brush.at(x, y) != 0)
      {
        field->at(pos.x + x, pos.y + y) = brush.at(x, y);
      }
    }
}

void
TilemapLayer::set_draw_attribute(bool t)
{
  impl->draw_attribute = t;
}

bool
TilemapLayer::get_draw_attribute() const
{
  return impl->draw_attribute;
}

void
TilemapLayer::set_draw_grid(bool t)
{
  impl->draw_grid = t;
}

bool
TilemapLayer::get_draw_grid() const
{
  return impl->draw_grid;
}

PixelBuffer
TilemapLayer::create_pixelbuffer()
{
  int tile_size = impl->tileset.get_tile_size();

  PixelBuffer pixelbuffer(get_width()  * tile_size, get_height() * tile_size);

  {
    pixelbuffer.lock();
    unsigned char* buf = static_cast<unsigned char*>(pixelbuffer.get_data());

    int width  = pixelbuffer.get_width();
    int height = pixelbuffer.get_height();

    // Draw a nice gradient
    for(int y = 0; y < height; ++y)
    {
      for (int x = 0; x < width; ++x)
      {
        buf[4*(y*width + x) + 0] = 255;
        buf[4*(y*width + x) + 1] = 255;
        buf[4*(y*width + x) + 2] = 255*y/height;
        buf[4*(y*width + x) + 3] = 255*y/height;
      }
    }
    pixelbuffer.unlock();
  }

  for (int y = 0; y < get_height(); ++y)
    for (int x = 0; x < get_width(); ++x)
    {
      Tile* tile = impl->tileset.create(impl->field.at(x, y));

      if (tile)
      {
        PixelBuffer buf = tile->get_pixelbuffer();
        if (buf)
        {
          blit(pixelbuffer, buf, x*tile_size, y*tile_size);
        }
      }
    }

  return pixelbuffer;
}

Rect
TilemapLayer::get_bounding_rect() const
{
  return impl->get_bounding_rect();
}

Rect
TilemapLayerImpl::get_bounding_rect() const
{
  return Rect(Point(0, 0),
              Size(field.get_width()  * tileset.get_tile_size(),
                   field.get_height() * tileset.get_tile_size()));
}

Point
TilemapLayer::world2tile(const Pointf& pos) const
{
  int x = static_cast<int>(pos.x / impl->tileset.get_tile_size());
  int y = static_cast<int>(pos.y / impl->tileset.get_tile_size());

  return Point(pos.x < 0 ? x-1 : x,
                  pos.y < 0 ? y-1 : y);
}

Field<int>*
TilemapLayer::get_field()
{
  return &impl->field;
}

TilemapLayer
TilemapLayer::current()
{
  return current_;
}

void
TilemapLayer::set_current(TilemapLayer t)
{
  current_ = t;
}

Tileset
TilemapLayer::get_tileset()
{
  return impl->tileset;
}

const std::vector<int>&
TilemapLayer::get_data()
{
  return impl->field.get_data();
}

void
TilemapLayer::set_data(std::vector<int> d)
{
  impl->field.set_data(d);
}

void
TilemapLayer::set_background_color(const Color& color)
{
  impl->background_color = color;
}

void
TilemapLayer::set_foreground_color(const Color& color)
{
  impl->foreground_color = color;
}

int
TilemapLayer::get_width() const
{
  return impl->field.get_width();
}

int
TilemapLayer::get_height() const
{
  return impl->field.get_height();
}

bool
TilemapLayer::has_bounding_rect() const
{
  return impl->has_bounding_rect();
}

bool
TilemapLayerImpl::has_bounding_rect() const
{
  return true;
}

Layer
TilemapLayer::to_layer()
{
  return Layer(impl);
}

void
TilemapLayer::set_metadata(const MetaData& metadata)
{
  impl->metadata = metadata;
}

MetaData
TilemapLayer::get_metadata() const
{
  return impl->metadata;
}

/* EOF */
