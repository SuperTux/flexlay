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

#ifndef HEADER_TILE_SELECTOR_WIDGET_HPP
#define HEADER_TILE_SELECTOR_WIDGET_HPP

#include <QWidget>

#include "tileset.hpp"
#include "math/rect.hpp"

class TileSelectorWidget : public QWidget
{
  Q_OBJECT
public:
  typedef std::vector<int> Tiles;

private:
  QWidget* m_viewport;

  int m_columns;
  int index;

  int offset;
  int old_offset;
  int mouse_over_tile;
  bool scrolling;
  bool region_select;
  Point current_pos;
  Point region_select_start;
  Point mouse_pos;
  float scale;

  /** set of tiles that should be available in the TileSelector */
  Tiles m_tiles;

  Tileset m_tileset;

public:
  TileSelectorWidget(QWidget* viewport);
  ~TileSelectorWidget();

  /** Set the factor by which tiles are scaled down in the selector
      widged (ie. for better overview) */
  void set_scale(float s);

  Tiles get_tiles() const;
  void set_tileset(Tileset t);
  void set_tiles(const Tiles& t);

protected:
  void mouseReleaseEvent(QMouseEvent* event) override;
  void mousePressEvent(QMouseEvent* event) override;
  void mouseMoveEvent(QMouseEvent* event) override;
  void wheelEvent(QWheelEvent* event) override;

  void paintEvent(QPaintEvent* event) override;
  void resizeEvent(QResizeEvent* event) override;
  QSize minimumSizeHint() const override;

private:
  Point get_mouse_tile_pos(const Point& mouse_pos);
  Rect get_selection();
  int get_columns() const;

private:
  TileSelectorWidget(const TileSelectorWidget&) = delete;
  TileSelectorWidget& operator=(const TileSelectorWidget&) = delete;
};

#endif

/* EOF */
