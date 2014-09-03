//  Flexlay - A Generic 2D Game Editor
//  Copyright (C) 2000 Ingo Ruhnke <grumbel@gmx.de>
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

#include <ClanLib/gui.h>

#include "../tileset.hpp"
#include "math/point.hpp"
#include "math/rect.hpp"

class Tileset;

class TileSelector : public CL_Component
{
public:
  typedef std::vector<int> Tiles;

private:
  CL_SlotContainer slots;
  int width;
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
  Tiles tiles;

  Tileset tileset;

protected:
  virtual ~TileSelector();
public:
  /** width and height in number of tiles */
  TileSelector(const Rect& rect, CL_Component* parent);

  void set_tileset(Tileset t);
  void set_tiles(const Tiles& t);
  Tiles get_tiles() const;

  /** Set the factor by which tiles are scaled down in the selector
      widged (ie. for better overview) */
  void set_scale(float s);

  void draw();

  /** Return the position of the mouse in x/y in tilesize */
  Point get_mouse_tile_pos(const CL_InputEvent& event);

private:
  Rect get_selection();

  void mouse_move(const CL_InputEvent& event);
  void mouse_down(const CL_InputEvent& event);
  void mouse_up  (const CL_InputEvent& event);
};

#endif

/* EOF */
