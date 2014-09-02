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

#ifndef HEADER_FLEXLAY_TILE_SELECTION_HPP
#define HEADER_FLEXLAY_TILE_SELECTION_HPP

#include <ClanLib/Display/color.h>

#include "../tile_brush.hpp"
#include "../tilemap_layer.hpp"

class TileSelectionImpl;

/** The TileSelection is a little helper class to manage rectangular
    selections of tiles and provides a way to convert this selection
    to a Brush which then can be used for either serialisation or be
    used for painting on the map itself */
class TileSelection
{
public:
  TileSelection();
  ~TileSelection();

  void start (TilemapLayer tilemap, const Point& pos);
  void update(const Point& pos);

  void clear();
  bool is_active();

  Rect get_rect() const;

  void draw(const CL_Color& color = CL_Color(255, 255, 255, 100));

  TileBrush get_brush(const Field<int>& field) const;
private:
  std::shared_ptr<TileSelectionImpl> impl;
};

#endif

/* EOF */
