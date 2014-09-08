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

#include "gui/tile_selector_widget.hpp"

#include <QPainter>
#include <QResizeEvent>
#include <iostream>

#include "graphic_context.hpp"
#include "math.hpp"
#include "sprite.hpp"
#include "tile.hpp"
#include "tile_brush.hpp"
#include "tools/tilemap_paint_tool.hpp"

TileSelectorWidget::TileSelectorWidget() :
  width(1),
  index(0),
  offset(0),
  old_offset(),
  mouse_over_tile(-1),
  scrolling(false),
  region_select(false),
  current_pos(),
  region_select_start(),
  mouse_pos(),
  scale(1.0f)
{
}

TileSelectorWidget::~TileSelectorWidget()
{
  std::cout << "~TileSelectorWidget()" << std::endl;
}

QSize
TileSelectorWidget::minimumSizeHint() const
{
  return QSize(32, 32);
}
  
QSize
TileSelectorWidget::sizeHint() const
{
  return QSize(32 * 6, 32 * 10);
}

Rect
TileSelectorWidget::get_selection()
{
  Rect selection(current_pos.x, current_pos.y,
                 region_select_start.x, region_select_start.y);

  selection.normalize();
  selection.right  += 1;
  selection.bottom += 1;

  selection.left  = Math::mid(0, selection.left, width);
  selection.right = Math::mid(0, selection.right, width);

  selection.top    = Math::max(0, selection.top);

  return selection;
}

void
TileSelectorWidget::mousePressEvent(QMouseEvent* event)
{
  switch(event->button())
  {
    case Qt::LeftButton:
      {
        TileBrush brush(1, 1);

        brush.set_opaque();
        if (mouse_over_tile >= 0 && mouse_over_tile < int(tiles.size()))
          brush.at(0, 0) = tiles[mouse_over_tile];
        else
          brush.at(0, 0) = 0;

        TileMapPaintTool::current().set_brush(brush);
      }
      break;

    case Qt::RightButton:
      region_select = true;
      region_select_start = current_pos;
      grabMouse();
      break;

    case Qt::MidButton:
      scrolling = true;
      mouse_pos = Point(event->pos());
      old_offset = offset;
      grabMouse();
      break;

    default:
      break;
  }

  repaint();
}

void
TileSelectorWidget::mouseReleaseEvent(QMouseEvent* event)
{
  switch(event->button())
  {
    case Qt::MidButton:
      scrolling = false;
      releaseMouse();
      break;

    case Qt::RightButton:
      {
        releaseMouse();
        region_select = false;

        Rect selection = get_selection();
        //selection.bottom = Math::mid(0, selection.right, width);

        TileBrush brush(selection.get_width(), selection.get_height());
        brush.set_transparent();

        for(int y = 0; y < selection.get_height(); ++y)
          for(int x = 0; x < selection.get_width(); ++x)
          {
            int tile = (selection.top + y) * width + (selection.left + x);

            if (tile >= 0 && tile < int(tiles.size()))
              brush.at(x, y) = tiles[tile];
            else
              brush.at(x, y) = 0;
          }

        TileMapPaintTool::current().set_brush(brush);
      }
      break;

    default:
      break;
  }

  repaint();
}

void
TileSelectorWidget::mouseMoveEvent(QMouseEvent* event)
{
  Point pos = get_mouse_tile_pos(Point(event->pos()));
  current_pos = pos;
  mouse_over_tile = pos.y * width + pos.x;

  if (scrolling)
  {
    offset = old_offset + (mouse_pos.y - event->y());
    if (offset < 0)
      offset = 0;
  }

  repaint();
}

void
TileSelectorWidget::wheelEvent(QWheelEvent* event)
{
  int numDegrees = event->delta() / 8;
  int numSteps = numDegrees / 15;

  offset += static_cast<int>(tileset.get_tile_size()*scale) * numSteps;
  
  if (offset < 0)
  {
    offset = 0;
  }
}

Point
TileSelectorWidget::get_mouse_tile_pos(const Point& mouse_pos)
{
  return Point(mouse_pos.x/static_cast<int>(tileset.get_tile_size()*scale),
              (mouse_pos.y+offset)/static_cast<int>(tileset.get_tile_size()*scale));
}

void
TileSelectorWidget::paintEvent(QPaintEvent* event)
{
  QPainter painter(this);
  GraphicContext gc(painter);

  gc.push_modelview();
  ////gc.translate(get_screen_x(), get_screen_y());
  gc.translate(0, -offset);

  const TileBrush& brush = TileMapPaintTool::current().get_brush();

  int start_row = offset / int(tileset.get_tile_size() * scale);
  int end_row   = start_row + (height() / int(tileset.get_tile_size() * scale));
  int end_index = std::min(end_row*width, int(tiles.size()));

  // Draw tiles
  for(int i = (start_row*width); i < end_index; ++i)
  {
    int x = i % width;
    int y = i / width;

    Tile* tile = tileset.create(tiles[i]);

    Rect rect(Point(static_cast<int>(x * tileset.get_tile_size()*scale),
                          static_cast<int>(y * tileset.get_tile_size()*scale)),
                 Size(static_cast<int>(tileset.get_tile_size()*scale),
                         static_cast<int>(tileset.get_tile_size()*scale)));

    if (tile)
    {
      Sprite sprite = tile->get_sprite();

      sprite.set_scale(scale, scale);

      sprite.draw(static_cast<int>(x * tileset.get_tile_size()*scale),
                  static_cast<int>(y * tileset.get_tile_size()*scale), gc);

      // Use grid in the tileselector
      //gc.draw_rect(rect.to_cl(), Color(0,0,0,128));
    }

    if (brush.get_width() == 1 && brush.get_height() == 1
        && brush.at(0, 0) == tiles[i])
    {
      gc.fill_rect(rect, Color(0,0,255, 100));
    }
    else if (mouse_over_tile == int(i)) ////GRUMBEL && has_mouse_over())
    {
      gc.fill_rect(rect, Color(0,0,255, 20));
    }
  }

  if (region_select)
  {
    Rect rect = get_selection();

    rect.top    *= static_cast<int>(tileset.get_tile_size()*scale);
    rect.bottom *= static_cast<int>(tileset.get_tile_size()*scale);
    rect.left   *= static_cast<int>(tileset.get_tile_size()*scale);
    rect.right  *= static_cast<int>(tileset.get_tile_size()*scale);

    gc.fill_rect(rect, Color(0,0,255, 100));
  }

  gc.pop_modelview();
}

void
TileSelectorWidget::resizeEvent(QResizeEvent* event)
{
  repaint();
}

void
TileSelectorWidget::set_scale(float s)
{
  scale = s;
  width  = static_cast<int>(size().width() / (tileset.get_tile_size() * scale));
}

TileSelectorWidget::Tiles
TileSelectorWidget::get_tiles() const
{
  return tiles;
}

void
TileSelectorWidget::set_tileset(Tileset t)
{
  tileset = t;
  // Recalc the number of tiles in a row
  width  = static_cast<int>(size().width()/(tileset.get_tile_size() * scale));
}

void
TileSelectorWidget::set_tiles(const Tiles& t)
{
  tiles = t;
  offset = 0;
}

/* EOF */
