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

#ifndef HEADER_FLEXLAY_TILE_EDITOR_HPP
#define HEADER_FLEXLAY_TILE_EDITOR_HPP

#include <ClanLib/GUI/component.h>

#include "math/point.hpp"

class Tile;

class TileEditor : public CL_Component
{
private:
  Tile* tile;
  CL_SlotContainer slots;
  Point mouse_pos;
protected:
  virtual ~TileEditor();
public:
  TileEditor(int x, int y, int w, int h, CL_Component* parent);

  void draw();
  void mouse_move(const CL_InputEvent& event);
  void mouse_down(const CL_InputEvent& event);
  void mouse_up  (const CL_InputEvent& event);

  void set_tile(Tile* tile);
private:
  void paint(Point pos, bool val);

  TileEditor (const TileEditor&);
  TileEditor& operator= (const TileEditor&);
};

#endif

/* EOF */
