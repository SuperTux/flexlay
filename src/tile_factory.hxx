//  $Id: tile_factory.hxx,v 1.3 2003/08/11 11:18:11 grumbel Exp $
// 
//  Pingus - A free Lemmings clone
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

#ifndef TILEFACTORY_HXX
#define TILEFACTORY_HXX

#include <map>
#include <string>

class Tile;

/** */
class TileFactory
{
private:
  std::map<std::string, Tile*> tiles;

  static TileFactory* current_;
public:
  /** Create a TileFactory from a given tile definition file */
  TileFactory (const std::string& filename);

  Tile* create(int id);

  /** Create the default TileFactor*/
  static void init();

  /** Destroy the default TileFactor*/
  static void deinit();

  /** Access the default TileFactor*/
  static TileFactory* current() { return current_; }

private:
  void parse_tile(SCM data);
};

#endif

/* EOF */
