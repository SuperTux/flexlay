//  $Id: tilemap_paint_tool.hxx,v 1.1 2003/09/23 19:10:05 grumbel Exp $
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

#ifndef HEADER_TILEMAP_PAINT_TOOL_HXX
#define HEADER_TILEMAP_PAINT_TOOL_HXX

#include "tool.hxx"
#include "tile_brush.hxx"
#include "tilemap_layer.hxx"

class TileMapPaintToolImpl;

/** */
class TileMapPaintTool
{
private:
  static TileMapPaintTool current_; 
public:
  static TileMapPaintTool current() { return current_; } 

  TileMapPaintTool();
  ~TileMapPaintTool();
  
  const TileBrush& get_brush();
  void set_brush(const TileBrush& b);

  Tool to_tool();
private:
  SharedPtr<TileMapPaintToolImpl> impl;
};

#endif

/* EOF */
