//  $Id$
// 
//  Pingus - A free Lemmings clone
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

#ifndef HEADER_SCRIPTING_NETPANZER_HXX
#define HEADER_SCRIPTING_NETPANZER_HXX

#include <string>
#include <ClanLib/Display/palette.h>
#include <ClanLib/Display/surface.h>
#include <ClanLib/Display/sprite.h>
#include "../lib/tileset.hxx"
#include "../lib/tilemap_layer.hxx"
#include "../lib/shared_ptr.hxx"

void load_netpanzer_tileset(Tileset tileset, const char* filename);

class NetPanzerFileStructImpl;

struct NetPanzerTileHeader
{
public:
  char	attrib;
  char	move_value;
  char	avg_color;
};

struct NetPanzerTileGroup
{
  int start;
  int width;
  int height;
  
  CL_Surface get_surface(); 

private:
  /** Surface holding the tilegroups image */
  CL_Surface surface;
};

class NetPanzerData
{
private:
  static NetPanzerData* instance_;
public:
  static NetPanzerData* instance() 
  {
    if (instance_)
      return (instance_);
    else
      return (instance_ = new NetPanzerData());
  }

private:
  std::string    datadir;
  CL_Palette     palette;
  Tileset        tileset;
  unsigned char* tiledata;
  typedef std::vector<NetPanzerTileGroup> TileGroups;
  TileGroups tilegroups;

public:
  NetPanzerData();

  /** Register a tilegroup, ie. a section of tiles that belong
      together and form an building, lake, a section of trees, etc.

      \param start the tile-id for the upper/left tile
      \param width the width of tilegroup
      \param height the height of the tilegroup
  */
  void register_tilegroup(int start, int width, int height);

  void load_data(const std::string& datadir_);
  const CL_Palette& get_palette() const;
  const Tileset&    get_tileset() const;
  unsigned char*    get_tiledata() const;

  CL_Palette load_palette(const std::string& filename);
  Tileset    load_tileset(const std::string& filename);
  CL_Sprite  get_tilegroup_sprite(int index);

  /** Locate the tilegroup in which the tile with \a tileindex is
      located */
  NetPanzerTileGroup& find_tilegroup(int tileindex);
};

class NetPanzerFileStruct
{
public:
  NetPanzerFileStruct(Tileset tileset, int w, int h);
  NetPanzerFileStruct(Tileset tileset, const std::string& filename);

  std::string get_id_header();
  std::string get_name();
  std::string get_description();
  TilemapLayer get_tilemap(); 

  void set_id_header(const std::string& id);
  void set_name(const std::string& name);
  void set_description(const std::string& description);
  void set_tilemap(TilemapLayer l); 

  void save(const std::string& filename);
private:
  SharedPtr<NetPanzerFileStructImpl> impl;
};

#endif

/* EOF */
