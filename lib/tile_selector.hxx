//  $Id: tile_selector.hxx,v 1.2 2003/09/10 13:53:11 grumbel Exp $
// 
//  Flexlay - A Generic 2D Game Editor
//  Copyright (C) 2000 Ingo Ruhnke <grumbel@gmx.de>
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

#ifndef TILESELECTOR_HXX
#define TILESELECTOR_HXX

#include <ClanLib/gui.h>
#include "tileset.hxx"

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
  CL_Point current_pos;
  CL_Point region_select_start;
  CL_Point mouse_pos;
  float scale;
  
  /** set of tiles that should be available in the TileSelector */
  Tiles tiles;

  Tileset tileset;

protected:
  virtual ~TileSelector();
public:
  /** width and height in number of tiles */
  TileSelector(const CL_Rect& rect, CL_Component* parent);
  
  void set_tileset(Tileset t);
  void set_tiles(const Tiles& t);
  Tiles get_tiles() const;
  
  /** Set the factor by which tiles are scaled down in the selector
      widged (ie. for better overview) */
  void set_scale(float s);

  void draw();

  /** Return the position of the mouse in x/y in tilesize */
  CL_Point get_mouse_tile_pos(const CL_InputEvent& event);

private:
  CL_Rect get_selection();

  void mouse_move(const CL_InputEvent& event);
  void mouse_down(const CL_InputEvent& event);
  void mouse_up  (const CL_InputEvent& event);
};

#endif

/* EOF */
