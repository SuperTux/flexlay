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

#include <iostream>
#include <sstream>
#include <fstream>
#include <ClanLib/core.h>
#include <ClanLib/Display/palette.h>
#include <ClanLib/Display/sprite.h>
#include <ClanLib/Display/pixel_format.h>
#include <ClanLib/Display/pixel_format_type.h>
#include <ClanLib/Display/sprite_description.h>
#include <ClanLib/Display/Providers/provider_factory.h>
#include "globals.hxx"
#include "tile_provider.hxx"
#include "tile_provider_impl.hxx"
#include "tile.hxx"
#include "tileset.hxx"
#include "blitter.hxx"
#include "tilemap_layer.hxx"
#include "editor_map.hxx"
#include "netpanzer.hxx"

NetPanzerData* NetPanzerData::instance_ = 0;

class NetPanzerTileProviderImpl : public TileProviderImpl
{
private:
  int id;
  mutable CL_Sprite sprite;
  mutable CL_PixelBuffer buffer;

public:
  NetPanzerTileProviderImpl(int id_)
    : id(id_)
  {    
  }

  virtual ~NetPanzerTileProviderImpl()
  {
  }

  CL_Sprite get_sprite() const
  {
    if (sprite)
      {
        return sprite;
      }
    else
      {
        NetPanzerTileGroup& tilegroup = NetPanzerData::instance()->find_tilegroup(id);
        int dist = id - tilegroup.start;

        CL_Rect rect(CL_Point((dist % tilegroup.width) * 32,
                              (dist / tilegroup.width) * 32),
                     CL_Size(32, 32));
        sprite.add_frame(tilegroup.get_surface(), rect);
        return sprite;
      }
  }

  CL_PixelBuffer get_pixelbuffer() const
  {
    if (buffer)
      {
        return buffer;
      }
    else
      {
        // FIXME: ClanLibs indexed handling seems broken, so we do
        // the conversion ourself
        const CL_Palette& palette = NetPanzerData::instance()->get_palette();
        unsigned char* data = NetPanzerData::instance()->get_tiledata() + (32*32) * id;
        buffer = CL_PixelBuffer(32, 32, 32*3, CL_PixelFormat::rgb888);

        buffer.lock();
        unsigned char* target = static_cast<unsigned char*>(buffer.get_data());

        for(int i = 0; i < 32*32; ++i)
          {
            target[3*i+0] = palette[data[i]].get_blue();
            target[3*i+1] = palette[data[i]].get_green();
            target[3*i+2] = palette[data[i]].get_red();
          }
        buffer.unlock();
                
        return buffer;
      }
  }
};

NetPanzerData::NetPanzerData() 
{
}

void
NetPanzerData::register_tilegroup(int start, int width, int height)
{
  NetPanzerTileGroup group;

  group.start  = start;
  group.width  = width;
  group.height = height;

  tilegroups.push_back(group);
}

void
NetPanzerData::load_data(const std::string& datadir_)
{
  datadir = datadir_;
  std::cout << "NetPanzerData: Loading data from '" << datadir << "'" << std::endl;
  palette = load_palette(datadir + "/" + "wads/netp.act");
  tileset = load_tileset(datadir + "/" + "wads/summer12mb.tls");
}

CL_Surface
NetPanzerTileGroup::get_surface()
{
  if (!surface)
    {
      CL_PixelBuffer buffer(width*32, height*32, width*32*4, CL_PixelFormat::rgba8888);

      for(int y = 0; y < height; ++y)
        for(int x = 0; x < width; ++x)
          {
            const CL_Palette& palette = NetPanzerData::instance()->get_palette();
            unsigned char* data = NetPanzerData::instance()->get_tiledata() + (32*32) * (start + width*y + x);
            
            CL_PixelBuffer tile(32, 32, 32*3, CL_PixelFormat::rgb888);
            tile.lock();
            unsigned char* target = static_cast<unsigned char*>(tile.get_data());

            for(int i = 0; i < 32*32; ++i)
              {
                target[3*i+0] = palette[data[i]].get_blue();
                target[3*i+1] = palette[data[i]].get_green();
                target[3*i+2] = palette[data[i]].get_red();
              }
            tile.unlock();
                
            blit(buffer, tile, x * 32, y * 32);
          }
      
      surface = CL_Surface(buffer);
    }

  return surface;
}

CL_Sprite
NetPanzerData::get_tilegroup_sprite(int index)
{
  for(TileGroups::iterator i = tilegroups.begin(); i != tilegroups.end(); ++i)
    {
      if (index == i->start)
        {
          CL_Sprite sprite;
          sprite.add_frame(i->get_surface(), CL_Rect(CL_Point(0, 0),
                                                     CL_Size(i->get_surface().get_width(),
                                                             i->get_surface().get_height())));
          return sprite;
        }
    }

  std::cout << "NetPanzerData: Couldn't get tilegroup_sprite for '" << index << "'" << std::endl;
  return CL_Sprite();
}

NetPanzerTileGroup&
NetPanzerData::find_tilegroup(int index)
{
  for(TileGroups::iterator i = tilegroups.begin(); i != tilegroups.end(); ++i)
    {
      if (i->start <= index && index < i->start + (i->width*i->height))
        {
          return *i;
        }
    }

  std::cout << "NetPanzerData: Couldn't find tilegroup for '" << index << "'" << std::endl;
  // return some junk just to keep it running
  return tilegroups.front();
}

const Tileset&
NetPanzerData::get_tileset() const
{
  return tileset;
}

const CL_Palette&
NetPanzerData::get_palette() const
{
  return palette;
}

unsigned char*
NetPanzerData::get_tiledata() const
{
  return tiledata;
}

CL_Palette
NetPanzerData::load_palette(const std::string& filename)
{
  CL_Palette palette;
  unsigned char color_array[256 * 3];
  
  std::ifstream in(filename.c_str());
  
  if (!in)
    {
      std::cout << "Couldn't load palette" << std::endl;
      return palette;
    }

  in.read(reinterpret_cast<char*>(color_array), sizeof(color_array));

  for(int i = 0; i < 256; ++i)
    {
      palette.colors[i].set_red  (color_array[3*i + 0]);
      palette.colors[i].set_green(color_array[3*i + 1]);
      palette.colors[i].set_blue (color_array[3*i + 2]);
    }

  return palette;
}

Tileset
NetPanzerData::load_tileset(const std::string& filename)
{
  unsigned char	netp_id_header[64];
  unsigned short	version;
  unsigned short	width;
  unsigned short	height;
  unsigned short	tile_count;
  unsigned char	raw_palette[768];

  std::ifstream file(filename.c_str());  

  if (!file)
    {
      std::cout << "Couldn't load " << filename << std::endl;
      return Tileset();
    }
  else
    {
      file.read(reinterpret_cast<char*>(netp_id_header), sizeof(netp_id_header));
      file.read(reinterpret_cast<char*>(&version), sizeof(version));
      file.read(reinterpret_cast<char*>(&width), sizeof(width));
      file.read(reinterpret_cast<char*>(&height), sizeof(height));
      file.read(reinterpret_cast<char*>(&tile_count), sizeof(tile_count));
      file.read(reinterpret_cast<char*>(raw_palette), sizeof(raw_palette));

      NetPanzerTileHeader* tile_headers = new NetPanzerTileHeader[tile_count];

      file.read(reinterpret_cast<char*>(tile_headers), 
                sizeof(NetPanzerTileHeader)*tile_count);

      cl_uint32 tilesize = width * height;
      // FIXME: Delete this somewhere!
      unsigned char* tiledata = new unsigned char[tilesize*tile_count];
      file.read(reinterpret_cast<char*>(tiledata), tilesize*tile_count);
      file.close();

      // FIXME: The palette in the netpanzer 'summer12mb.tls' file
      // is either broken or otherwise corrupt, so we ignore it
      // and use the seperate palette file 'netp.act' which works
      // fine.
      
      NetPanzerData::instance()->tiledata = tiledata;
      
      Tileset tileset(width);

      for(int i = 0; i < tile_count; ++i)
        {
          Tile tile(TileProvider(new NetPanzerTileProviderImpl(i)));
          tileset.add_tile(i, &tile);
        }
      return tileset;
    }
}

unsigned char find_nearest_color(const CL_Palette& palette, const CL_Color& rgb)
{ // Copyright (C) 1998 Pyrosoft Inc. (www.pyrosoftgames.com), Matthew Bogue
  float bestDist = 10000000.0f;
  int   best     = 0;

  float vPic = sqrt(rgb.get_red() * rgb.get_red() 
                    + rgb.get_green() * rgb.get_green()
                    + rgb.get_blue() * rgb.get_blue()) * 0.57735027;
 
  for (int i = 0; i < 256; i++) {
    float vPal = sqrt(palette.colors[i].get_red()     * palette.colors[i].get_red()
                      + palette.colors[i].get_green() * palette.colors[i].get_green()
                      + palette.colors[i].get_blue()  * palette.colors[i].get_blue()) * 0.57735027;

    float dr = palette.colors[i].get_red()   - rgb.get_red();
    float dg = palette.colors[i].get_green() - rgb.get_green();
    float db = palette.colors[i].get_blue()  - rgb.get_blue();
    float dv = vPal-vPic;
    float dist = dr * dr * 0.3 + dg * dg * 0.59 + db * db * 0.11 + dv * dv * 0.7;

    if (dist < bestDist) {
      bestDist = dist;
      best = i;
    }
  }

  return best;
}

class NetPanzerFileStructImpl
{
public:
  std::string id_header;
  std::string name;
  std::string description;
  TilemapLayer tilemap; 
  Tileset tileset;
};

std::string
NetPanzerFileStruct::get_id_header()
{
  return impl->id_header;
}

std::string
NetPanzerFileStruct::get_name()
{
  return impl->name;
}

std::string
NetPanzerFileStruct::get_description()
{
  return impl->description;
}

TilemapLayer
NetPanzerFileStruct::get_tilemap()
{
  return impl->tilemap;
}

void
NetPanzerFileStruct::set_id_header(const std::string& id)
{
  impl->id_header = id;
}

void
NetPanzerFileStruct::set_name(const std::string& name)
{
  impl->name = name;
}

void
NetPanzerFileStruct::set_description(const std::string& description)
{
  impl->description = description;
}

void
NetPanzerFileStruct::set_tilemap(TilemapLayer l)
{
  impl->tilemap = l;
}


void
NetPanzerFileStruct::save(const std::string& filename)
{
  if (impl->tilemap.is_null())
    return;
    
  unsigned char   netp_id_header[64];
  strcpy(reinterpret_cast<char*>(netp_id_header), impl->id_header.c_str());
  unsigned short  id       = 0; // ?
  char   name[256];
  strcpy(name, impl->name.c_str());
  char   description[1024];
  strcpy(description, impl->description.c_str());
  unsigned short  x_size   = impl->tilemap.get_width();
  unsigned short  y_size   = impl->tilemap.get_height();
  char            tile_set[256] = "summer12mb.tls";
 
  unsigned short  thumbnail_x_pix = impl->tilemap.get_width();
  unsigned short  thumbnail_y_pix = impl->tilemap.get_height();
    
  std::ofstream out(filename.c_str());

  // FIXME: Not endian clean
  out.write(reinterpret_cast<char*>(&netp_id_header), sizeof(netp_id_header));
  out.write(reinterpret_cast<char*>(&id), sizeof(short));
  out.write(reinterpret_cast<char*>(&name), sizeof(name));
  out.write(reinterpret_cast<char*>(&description), sizeof(description));
  out.write(reinterpret_cast<char*>(&x_size), sizeof(short));
  out.write(reinterpret_cast<char*>(&y_size), sizeof(short));
  out.write(reinterpret_cast<char*>(&tile_set), sizeof(tile_set));
  out.write(reinterpret_cast<char*>(&thumbnail_x_pix), sizeof(short));
  out.write(reinterpret_cast<char*>(&thumbnail_y_pix), sizeof(short));

  std::vector<unsigned short> vec(x_size * y_size);

  Field<int>* field = impl->tilemap.get_field();
  for(int i = 0; i < x_size * y_size; ++i)
    {
      vec[i] = (*field)[i];
    }
  out.write(reinterpret_cast<char*>(&(*vec.begin())), 
            sizeof(unsigned short)*vec.size());

  // Generate thumbnail
  std::vector<unsigned char> thumbnail(x_size * y_size);
  for(int i = 0; i < int(thumbnail.size()); ++i)
    {
      Tile* tile = impl->tileset.create((*field)[i]);
      if (tile)
        thumbnail[i] = find_nearest_color(NetPanzerData::instance()->get_palette(), tile->get_color());
    }

  out.write(reinterpret_cast<char*>(&(*thumbnail.begin())), 
            sizeof(unsigned char)*thumbnail.size());
}

NetPanzerFileStruct::NetPanzerFileStruct(Tileset tileset, int w, int h)
  : impl(new NetPanzerFileStructImpl())
{
  impl->tileset = tileset;
  impl->id_header = "<Id Header>";
  impl->name = "<Name>";
  impl->description = "<Description>";
  impl->tilemap = TilemapLayer(tileset, w, h);
}

NetPanzerFileStruct::NetPanzerFileStruct(Tileset tileset, const std::string& filename)
  : impl(new NetPanzerFileStructImpl())
{
  impl->tileset = tileset;
  
  // FIXME: endian issues
  unsigned char   netp_id_header[64]; // Copyright PyroSoft Inc.  ...
  unsigned short  id; // What is this?
  char            name[256];
  char            description[1024];
  unsigned short  x_size; // width
  unsigned short  y_size; // height
  char            tile_set[256]; // name of the tileset: "summer12mb.tls"
 
  unsigned short  thumbnail_x_pix;
  unsigned short  thumbnail_y_pix;

  std::ifstream file(filename.c_str());

  if (!file)
    {
      std::cout << "NetPanzerFileStructImpl: Error: " << filename << std::endl;
    }

  file.read(reinterpret_cast<char*>(&netp_id_header), sizeof(netp_id_header));
  file.read(reinterpret_cast<char*>(&id), sizeof(short));
  file.read(reinterpret_cast<char*>(&name), sizeof(name));
  file.read(reinterpret_cast<char*>(&description), sizeof(description));
  file.read(reinterpret_cast<char*>(&x_size), sizeof(short));
  file.read(reinterpret_cast<char*>(&y_size), sizeof(short));
  file.read(reinterpret_cast<char*>(&tile_set), sizeof(tile_set));
  file.read(reinterpret_cast<char*>(&thumbnail_x_pix), sizeof(short));
  file.read(reinterpret_cast<char*>(&thumbnail_y_pix), sizeof(short));

  TilemapLayer tilemap(tileset, x_size, y_size);
  Field<int>* field      = tilemap.get_field();

  std::vector<unsigned short> vec;
  vec.resize(x_size * y_size);
  file.read(reinterpret_cast<char*>(&(*vec.begin())), sizeof(unsigned short)*vec.size());

  for(int i = 0; i < x_size*y_size; ++i)
    (*field)[i] = vec[i];

  std::cout << "Thumbnail: " << thumbnail_x_pix << " " << thumbnail_y_pix << std::endl;

  impl->tilemap     = tilemap;
  impl->id_header   = reinterpret_cast<char*>(netp_id_header);
  impl->name        = name;
  impl->description = description;
}

/* EOF */
