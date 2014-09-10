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

#ifndef HEADER_FLEXLAY_TILESET_HPP
#define HEADER_FLEXLAY_TILESET_HPP

#include <memory>
#include <string>
#include <vector>

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
  std::shared_ptr<TilesetImpl> impl;
};

#endif

/* EOF */
