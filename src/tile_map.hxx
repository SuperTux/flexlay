//  $Id: tile_map.hxx,v 1.6 2003/08/18 08:50:22 grumbel Exp $
// 
//  Windstille - A Jump'n Shoot Game
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

#ifndef TILEMAP_HXX
#define TILEMAP_HXX

#include <ClanLib/core.h>
#include <ClanLib/display.h>

#include "globals.hxx"
#include "field.hxx"

class WindstilleLevel;
class Tile;

class TileMap
{
private:
  Field<Tile*> field;
  typedef Field<Tile*>::iterator FieldIter;
public:
  TileMap (Field<int>* data);

  void update (float delta);
  void draw ();
  
  /** @return the type of ground at the given world coordinates */
  bool is_ground(float x, float y);

  /** @return the type of ground at the given subtile coordinates */
  bool get_pixel(int x, int y);
  
  int get_width () const { return field.get_width(); }
  int get_height () const { return field.get_height (); }

  int get_tile_size () const { return TILE_SIZE; }
};

#endif

/* EOF */
