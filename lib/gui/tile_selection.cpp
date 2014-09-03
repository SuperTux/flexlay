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

#include "tile_selection.hpp"

#include <ClanLib/Display/display.h>
#include <iostream>

#include "display.hpp"
#include "math.hpp"
#include "math/rect.hpp"
#include "tileset.hpp"

class TileSelectionImpl
{
public:
  TilemapLayer tilemap;
  Point start_pos;
  Rect  selection;
  bool active;
};

TileSelection::TileSelection()
  : impl(new TileSelectionImpl())
{
  impl->active = false;
}

TileSelection::~TileSelection()
{
}

void
TileSelection::start(TilemapLayer tilemap_, const Point& pos)
{
  impl->tilemap = tilemap_;
  impl->active = true;
  impl->start_pos = pos;
  update(impl->start_pos);
}

void
TileSelection::update(const Point& pos)
{
  impl->selection = Rect(std::min(impl->start_pos.x, pos.x),
                         std::min(impl->start_pos.y, pos.y),
                         std::max(impl->start_pos.x, pos.x) + 1,
                         std::max(impl->start_pos.y, pos.y) + 1);
}

bool
TileSelection::is_active()
{
  return impl->active;
}

void
TileSelection::clear()
{
  impl->selection = Rect();
  impl->active = false;
}

void
TileSelection::draw(const Color& color)
{
  int tile_size = impl->tilemap.get_tileset().get_tile_size();

  Display::fill_rect(Rect(impl->selection.left * tile_size,
                          impl->selection.top * tile_size,
                          impl->selection.right * tile_size,
                          impl->selection.bottom * tile_size),
                        color);
}

TileBrush
TileSelection::get_brush(const Field<int>& field) const
{
  Rect sel = impl->selection;

  sel.normalize();

  if (sel.left > field.get_width() - 1
      || sel.top > field.get_height() - 1
      || sel.right  <= 0
      || sel.bottom <= 0)
  { // Selection is empty
    std::cout << "Error: Invalid selection" << std::endl;
    TileBrush brush(1, 1);
    brush.at(0, 0) = 0;
    brush.set_opaque();
    return brush;
  }
  else
  { // Selection is valid
    // Cut the selection to the field size
    sel.left = Math::max(0, sel.left);
    sel.top  = Math::max(0, sel.top);

    sel.right  = Math::min(sel.right,  field.get_width());
    sel.bottom = Math::min(sel.bottom, field.get_height());

    TileBrush brush(sel.get_width(),
                    sel.get_height());

    for(int y = sel.top; y < sel.bottom; ++y)
      for(int x = sel.left; x < sel.right; ++x)
      {
        brush.at(x - sel.left,
                 y - sel.top) = field.at(x, y);
      }

    return brush;
  }
}

Rect
TileSelection::get_rect() const
{
  Rect sel = impl->selection;
  sel.normalize();
  return sel;
}

/* EOF */
