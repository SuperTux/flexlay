//  $Id: tile_factory.cxx,v 1.10 2003/09/22 18:37:05 grumbel Exp $
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

#include <string>
#include <ClanLib/Core/System/system.h>
#include <assert.h>
#include <iostream>
#include "globals.hxx"
#include "string_converter.hxx"
#include "tile.hxx"
#include "tileset.hxx"

typedef std::vector<Tile*> Tiles;
typedef std::vector<int> TileIds;
typedef Tiles::iterator iterator;
  
class TilesetImpl 
{
public:
  TilesetImpl()
  {
  }

  ~TilesetImpl()
  {
    for(Tiles::iterator i = tiles.begin(); i != tiles.end(); ++i)
      {
        delete *i;
      }
  }

  TileIds tile_ids;
  Tiles tiles;
  int tile_size;
};

Tileset::Tileset()
  : impl(new TilesetImpl())
{
  impl->tile_size = 1;
}

Tileset::Tileset(int tile_size_)
  : impl(new TilesetImpl())
{
  assert(tile_size_ > 0);
  impl->tile_size = tile_size_;
}

Tileset::~Tileset()
{
}

void
Tileset::add_tile(int id, Tile* tile)
{
  // FIXME: Check for tile-id dups
  if (id >= int(impl->tiles.size()))
    impl->tiles.resize(id+1, 0);

  if (tile)
    impl->tiles[id] = new Tile(*tile);
  else
    impl->tiles[id] = 0;

  impl->tile_ids.push_back(id);
}

Tile* 
Tileset::create (int id)
{
  if (id >= 0 && id < int(impl->tiles.size()))
    return impl->tiles[id];
  else
    return 0;
}

int
Tileset::get_tile_size() const 
{
  return impl->tile_size; 
}

std::vector<int> 
Tileset::get_tiles() const
{
  return impl->tile_ids;
}

/* EOF */
