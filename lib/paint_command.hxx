//  $Id$
// 
//  Flexlay - A Generic 2D Game Editor
//  Copyright (C) 2002 Ingo Ruhnke <grumbel@gmx.de>
//
//  This program is free software; you can redistribute it and/or
//  modify it under the terms of the GNU General Public License
//  as published by the Free Software Foundation; either version 2
//  of the License, or (at your option) any later version.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//  GNU General Public License for more details.
// 
//  You should have received a copy of the GNU General Public License
//  along with this program; if not, write to the Free Software
//  Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.

#ifndef HEADER_PAINT_COMMAND_HXX
#define HEADER_PAINT_COMMAND_HXX

#include <ClanLib/Core/Math/point.h>
#include "tile_brush.hxx"
#include "shared_ptr.hxx"
#include "tilemap_layer.hxx"
#include "command.hxx"

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
  
  void add_point(const CL_Point& pos);

  Command to_command();
private:
  SharedPtr<PaintCommandImpl> impl;
};

#endif

/* EOF */
