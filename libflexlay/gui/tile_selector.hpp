//  Flexlay - A Generic 2D Game Editor
//  Copyright (C) 2000 Ingo Ruhnke <grumbel@gmail.com>
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

#ifndef HEADER_FLEXLAY_TILE_SELECTOR_HPP
#define HEADER_FLEXLAY_TILE_SELECTOR_HPP

#include "../tileset.hpp"
#include "math/point.hpp"
#include "math/rect.hpp"

class QWidget;
class QScrollArea;
class TileSelectorWidget;
class Tileset;

class TileSelector
{
protected:
  virtual ~TileSelector();
public:
  typedef std::vector<int> Tiles;

public:
  TileSelector();

  void set_tileset(Tileset t);
  void set_tiles(const Tiles& t);
  Tiles get_tiles() const;

  void set_scale(float s);

  QWidget* get_widget() const;

private:
  Point get_mouse_tile_pos(const Point& mouse_pos);
  Rect get_selection();

private:
  TileSelectorWidget* m_widget;
  QScrollArea* m_scroll_area;
};

#endif

/* EOF */
