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

#ifndef HEADER_TILE_SELECTION_HXX
#define HEADER_TILE_SELECTION_HXX

#include <ClanLib/Core/Math/rect.h>
#include <ClanLib/Display/color.h>
#include <ClanLib/Core/Math/point.h>
#include "tile_brush.hxx"
#include "tilemap_layer.hxx"

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

  void start (TilemapLayer tilemap, const CL_Point& pos);
  void update(const CL_Point& pos);

  void clear();
  bool is_active();

  CL_Rect get_rect() const;

  void draw(const CL_Color& color = CL_Color(255, 255, 255, 100));

  TileBrush get_brush(const Field<int>& field) const;
private:
  SharedPtr<TileSelectionImpl> impl;
};

#endif

/* EOF */
