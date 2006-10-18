//  $Id: tile_factory.hxx,v 1.8 2003/09/22 18:37:05 grumbel Exp $
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

#ifndef TILESET_HXX
#define TILESET_HXX

#include <vector>
#include <string>
#include <ClanLib/Core/System/sharedptr.h>

class Tile;
class TilesetImpl;

/** A \a Tileset provides the mapping from an \a id to a \a Tile
    structure. It also contains information of the tile_size and other
    necesarry information that are needed to display a TileMap */
class Tileset
{
public:
  explicit Tileset();

  /** Create an empty Tileset, so that the user can add stuff via
      scripting to it */
  explicit Tileset(int tile_size_);

  ~Tileset();

  /** Check if the tile is already loaded and return it. If it is not
   *  already loaded, load it 
   *
   *  @param id The id of the tile to create as defined in the def. file
   *
   *  @return on success the tile is returned, on failure 0 */
  Tile* create(int id);

  int get_tile_size() const;
  
  void add_tile(int id, Tile* tile);

  /** Return the tiles which are available in this tileset */
  std::vector<int> get_tiles() const;

private:
  CL_SharedPtr<TilesetImpl> impl;
};

#endif

/* EOF */
