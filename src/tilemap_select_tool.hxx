//  $Id: tilemap_select_tool.hxx,v 1.1 2003/09/23 22:10:40 grumbel Exp $
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

#ifndef HEADER_TILEMAP_SELECT_TOOL_HXX
#define HEADER_TILEMAP_SELECT_TOOL_HXX

#include <ClanLib/Core/Math/rect.h>
#include <ClanLib/Core/Math/point.h>
#include "tool.hxx"
#include "tile_selection.hxx"

class TileMapSelectToolImpl;

/** */
class TileMapSelectTool
{
public:
  TileMapSelectTool();
  ~TileMapSelectTool();

  /** Convert the selection into a TileBrush */
  TileBrush get_selection() const;

  CL_Rect get_selection_rect() const;

  Tool to_tool();
private:
  SharedPtr<TileMapSelectToolImpl> impl;
};

#endif

/* EOF */
