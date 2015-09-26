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

#include "gui/minimap_widget.hpp"

#include <QPainter>
#include <QMouseEvent>
#include <iostream>

#include "display.hpp"
#include "editor_map.hpp"
#include "editor_map_component.hpp"
#include "workspace.hpp"
#include "pixel_buffer.hpp"
#include "tile.hpp"
#include "tilemap_layer.hpp"
#include "tileset.hpp"
#include "graphic_context.hpp"

MinimapWidget::MinimapWidget(EditorMapComponent* p) :
  m_drag_active(false),
  m_last_serial(-1),
  m_editor_map(false),
  m_parent(),
  m_minimap_surface()
{
  m_parent = p;
}

QSize
MinimapWidget::minimumSizeHint() const
{
  return QSize(0, 0);
}
  
QSize
MinimapWidget::sizeHint() const
{
  return QSize(300, 150);
}

void
MinimapWidget::paintEvent(QPaintEvent* event)
{
  if (!m_parent->get_workspace().get_map()) 
    return;

  if (!m_parent || m_parent->get_workspace().is_null())
    return;
  
  QPainter painter(this);
  GraphicContext gc(painter);

  gc.push_modelview();

  // FIXME: Do this only on map changes
  if (m_last_serial != m_parent->get_workspace().get_map().get_serial())
    // || editor_map != parent->get_workspace().get_map())
  {
    update_minimap();
    m_last_serial = m_parent->get_workspace().get_map().get_serial();
    m_editor_map  = m_parent->get_workspace().get_map();
  }

  if (1)
  { // Draw background color
    gc.fill_rect(Rect(Point(0, 0), Size(width(), height())),
                 Color(200, 200, 200, 225));
  }

  // FIXME: This doesn't work all that well
  TilemapLayer tilemap = TilemapLayer::current();

  if (!tilemap.is_null() && tilemap.get_height() != 0 && tilemap.get_width() != 0)
  {
    int tile_size = tilemap.get_tileset().get_tile_size();

    int map_width  = tilemap.get_width()  * tile_size;
    int map_height = tilemap.get_height() * tile_size;

    Size small_tile(tile_size * width() / map_width + 1,
                    tile_size * height() / map_height + 1);

    Field<int>* field = tilemap.get_field();

    // FIXME: No current tileset
    if (0)
    {
      for(int y = 0; y < field->get_height(); ++y)
        for(int x = 0; x < field->get_width(); ++x)
        {
          Tile* tile = tilemap.get_tileset().create(field->at(x, y));
          if (tile)
            gc.fill_rect(Rect(Point((x * tile_size) * width() / map_width,
                                    (y * tile_size) * height() / map_height),
                              Size(small_tile)),
                         tile->get_color());
          gc.flush();
        }
    }
#ifdef GRUMBEL
    m_minimap_surface.draw(Rect(Point(0, 0),
                                Size(width(), height())));
#else
    m_minimap_surface.draw(0, 0, gc);
#endif

    // Draw cursor
    Rect rect(m_parent->get_clip_rect());
    Rect screen_rect(Point(rect.left * width()  / map_width,
                           rect.top * height() / map_height),
                     Size(rect.get_width() * width() /map_width,
                          rect.get_height()* height()/map_height));
    gc.fill_rect(screen_rect,
                 Color(255, 255, 0, 50));
    gc.draw_rect(screen_rect,
                 Color(0, 0, 0));
  }

  gc.pop_modelview();
}

void
MinimapWidget::update_minimap()
{
  // FIXME: This doesn't work all that well
  TilemapLayer tilemap = TilemapLayer::current();

  if (!tilemap.is_null())
  {
    Field<int>* field = tilemap.get_field();

    PixelBuffer buffer(tilemap.get_width(), tilemap.get_height());

    int map_width  = tilemap.get_width();
    int map_height = tilemap.get_height();

    // FIXME: No Tileset::current()
    unsigned char* buf = static_cast<unsigned char*>(buffer.get_data());
    for(int y = 0; y < map_height; ++y)
      for(int x = 0; x < map_width; ++x)
      {
        Tile* tile = tilemap.get_tileset().create(field->at(x, y));
        if (tile)
        {
          buf[4*(x + y * map_width) + 3] = tile->get_color().get_red();
          buf[4*(x + y * map_width) + 2] = tile->get_color().get_green();
          buf[4*(x + y * map_width) + 1] = tile->get_color().get_blue();
          buf[4*(x + y * map_width) + 0] = tile->get_color().get_alpha();
        }
        else
        {
          buf[4*(x + y * map_width) + 0] = 0;
          buf[4*(x + y * map_width) + 1] = 0;
          buf[4*(x + y * map_width) + 2] = 0;
          buf[4*(x + y * map_width) + 3] = 0;
        }
      }

    m_minimap_surface = Sprite(buffer);
  }
}

void
MinimapWidget::mouseMoveEvent(QMouseEvent* event)
{
  // FIXME: This doesn't work all that well
  TilemapLayer tilemap = TilemapLayer::current();
  if (!tilemap.is_null())
  {
    int tile_size  = tilemap.get_tileset().get_tile_size();
    int map_width  = tilemap.get_width()  * tile_size;
    int map_height = tilemap.get_height() * tile_size;

    if (m_drag_active)
      m_parent->move_to(event->x() * map_width / width(),
                        event->y() * map_height / height());
  }

  repaint();
}

void
MinimapWidget::mousePressEvent(QMouseEvent* event)
{
  // FIXME: This doesn't work all that well
  TilemapLayer tilemap = TilemapLayer::current();
  if (!tilemap.is_null())
  {
    int tile_size  = tilemap.get_tileset().get_tile_size();
    int map_width  = tilemap.get_width()  * tile_size;
    int map_height = tilemap.get_height() * tile_size;

    m_parent->move_to(event->x() * map_width / width(),
                      event->y() * map_height / height());
    m_drag_active = true;
    grabMouse();
  }

  repaint();
}

void
MinimapWidget::mouseReleaseEvent(QMouseEvent* event)
{
  TilemapLayer tilemap = TilemapLayer::current();
  if (!tilemap.is_null())
  {
    m_drag_active = false;
    releaseMouse();
  }

  repaint();
}

/* EOF */
