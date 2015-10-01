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

#ifndef HEADER_FLEXLAY_PAINT_COMMAND_HPP
#define HEADER_FLEXLAY_PAINT_COMMAND_HPP

#include "tile_brush.hpp"
#include "tilemap_layer.hpp"
#include "command.hpp"

class PaintCommandImpl;

/** The PaintCommand provides functionality to draw onto an TileMap.
    The user needs to supply a brush and a map to draw to and the
    points to which should be drawn, undo, redo and the internals of
    drawing are handled by the PaintCommand itself. */
class PaintCommand
{
public:
  PaintCommand(TilemapLayer t, const TileBrush& b);
  ~PaintCommand();

  void add_point(const Point& pos);

  Command to_command();
private:
  std::shared_ptr<PaintCommandImpl> impl;
};

#endif

/* EOF */
