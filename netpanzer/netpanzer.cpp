//  $Id$
//
//  Pingus - A free Lemmings clone
//  Copyright (C) 2002 Ingo Ruhnke <grumbel@gmx.de>
//
//  This program is free software: you can redistribute it and/or modify
//  it under the terms of the GNU General Public License as published by
//  the Free Software Foundation, either version 3 of the License, or
//  (at your option) any later version.
//  
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//  GNU General Public License for more details.
//  
//  You should have received a copy of the GNU General Public License
//  along with this program.  If not, see <http://www.gnu.org/licenses/>.

#include <iostream>
#include <cstring>
#include <cstdint>
#include <vector>
#include <sstream>
#include <fstream>
#include <ClanLib/core.h>
#include <ClanLib/Display/palette.h>
#include <ClanLib/Display/sprite.h>
#include <ClanLib/Display/pixel_format.h>
#include <ClanLib/Display/pixel_format_type.h>
#include <ClanLib/Display/sprite_description.h>
#include <ClanLib/Display/Providers/provider_factory.h>
#include "globals.hpp"
#include "tile_provider.hpp"
#include "tile_provider_impl.hpp"
#include "tile.hpp"
#include "tileset.hpp"
#include "blitter.hpp"
#include "tilemap_layer.hpp"
#include "editor_map.hpp"
#include "netpanzer.hpp"


// netPanzer binary formats are little-endian.

// netPanzer binary formats are little-endian.
static uint16_t
read_le16(std::istream& in)
{
  unsigned char b[2] = {0, 0};
  in.read(reinterpret_cast<char*>(b), 2);
  return static_cast<uint16_t>(b[0] | (static_cast<uint16_t>(b[1]) << 8));
}

static void
write_le16(std::ostream& out, uint16_t v)
{
  unsigned char b[2];
  b[0] = static_cast<unsigned char>(v & 0xff);
  b[1] = static_cast<unsigned char>((v >> 8) & 0xff);
  out.write(reinterpret_cast<char*>(b), 2);
}

static void
read_le16_array(std::istream& in, std::vector<uint16_t>& data)
{
  for (std::size_t i = 0; i < data.size(); ++i)
    data[i] = read_le16(in);
}

static void
write_le16_array(std::ostream& out, const std::vector<uint16_t>& data)
{
  for (std::size_t i = 0; i < data.size(); ++i)
    write_le16(out, data[i]);
}

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
  : tiledata(0)
{
}

NetPanzerData::~NetPanzerData()
{
  clear_tiledata();
}

void
NetPanzerData::clear_tiledata()
{
  delete[] tiledata;
  tiledata = 0;
  tile_headers.clear();
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
  load_tileset(datadir + "/" + "wads/summer12mb.tls");
}

CL_Surface
NetPanzerTileGroup::get_surface()
{
  if (!NetPanzerData::instance()->get_tiledata())
    {
      // Missing tileset data - return an empty surface rather than segfault.
      return CL_Surface();
    }

  if (!surface)
    {
      CL_PixelBuffer buffer(width*32, height*32, width*32*4, CL_PixelFormat::rgba8888);

      for(int y = 0; y < height; ++y)
        for(int x = 0; x < width; ++x)
          {
            const CL_Palette& palette = NetPanzerData::instance()->get_palette();
            unsigned char* data = NetPanzerData::instance()->get_tiledata() + (32*32) * (start + width*y + x);

            NetPanzerTileHeader header = NetPanzerData::instance()->get_tile_headers()[start + width*y + x]; 
           
            CL_PixelBuffer tile(32, 32, 32*3, CL_PixelFormat::rgb888);

            tile.lock();
            unsigned char* target = static_cast<unsigned char*>(tile.get_data());

            float r = 1.0f;
            float g = 1.0f;
            float b = 1.0f;

            switch (header.move_value)
              {
              case 0: // streets, allowing faster movement
                r = 1.0f;
                g = 1.0f;
                b = 0.0f;
                break;

              case 1: // normal ground which allows to move
                r = 1.0f;
                g = 1.0f;
                b = 1.0f;
                break;

              case 4: // unpassable terrain
                r = 0.0f;
                g = 1.0;
                b = 0.0f;
                break;

              case 5: // water
                r = 0.0f;
                g = 0.0f;
                b = 1.0f;
                break;

              default:
                std::cout << "Unknown header value: " << int(header.move_value) << std::endl;
                break;
              }

            for(int i = 0; i < 32*32; ++i)
              {
                target[3*i+0] = int(palette[data[i]].get_blue()  * b);
                target[3*i+1] = int(palette[data[i]].get_green() * g);
                target[3*i+2] = int(palette[data[i]].get_red()   * r);
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
  // No game data loaded (missing NETPANZER_DATADIR / .tls / palette).
  // Return an empty sprite instead of dereferencing null tiledata.
  if (!tiledata || tile_headers.empty())
    {
      std::cout << "NetPanzerData: get_tilegroup_sprite(" << index
                << "): tile data not loaded (set NETPANZER_DATADIR)" << std::endl;
      return CL_Sprite();
    }

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
  // Avoid undefined behaviour when tilegroups is empty.
  if (tilegroups.empty())
    {
      static NetPanzerTileGroup dummy;
      dummy.start  = 0;
      dummy.width  = 1;
      dummy.height = 1;
      return dummy;
    }
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

const std::vector<NetPanzerTileHeader>&
NetPanzerData::get_tile_headers() const
{
  return tile_headers;
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

void
NetPanzerData::load_tileset(const std::string& filename)
{
  unsigned char	netp_id_header[64];
  unsigned short	version;
  unsigned short	width;
  unsigned short	height;
  unsigned short	tile_count;
  unsigned char	raw_palette[768];

  std::ifstream file(filename.c_str(), std::ios::binary);
  if (!file)
    {
      std::cout << "Couldn't load " << filename << std::endl;
      return;
    }

  file.read(reinterpret_cast<char*>(netp_id_header), sizeof(netp_id_header));
  version    = read_le16(file);
  width      = read_le16(file);
  height     = read_le16(file);
  tile_count = read_le16(file);
  file.read(reinterpret_cast<char*>(raw_palette), sizeof(raw_palette));

  if (!file)
    {
      std::cout << "NetPanzerData: truncated header in " << filename << std::endl;
      return;
    }

  // File format is little-endian; hosts that are not LE will need conversion.
  if (width == 0 || height == 0 || tile_count == 0)
    {
      std::cout << "NetPanzerData: invalid tileset dimensions in " << filename << std::endl;
      return;
    }

  std::vector<NetPanzerTileHeader> headers(tile_count);
  file.read(reinterpret_cast<char*>(&headers[0]),
            static_cast<std::streamsize>(sizeof(NetPanzerTileHeader) * tile_count));
  if (!file)
    {
      std::cout << "NetPanzerData: truncated tile headers in " << filename << std::endl;
      return;
    }

  const cl_uint32 tilesize = static_cast<cl_uint32>(width) * static_cast<cl_uint32>(height);
  const std::size_t data_bytes = static_cast<std::size_t>(tilesize) * static_cast<std::size_t>(tile_count);
  unsigned char* data = new unsigned char[data_bytes];
  file.read(reinterpret_cast<char*>(data), static_cast<std::streamsize>(data_bytes));
  if (!file)
    {
      std::cout << "NetPanzerData: truncated tile pixel data in " << filename << std::endl;
      delete[] data;
      return;
    }
  file.close();

  // The palette in summer12mb.tls is ignored; netp.act is used instead.

  clear_tiledata();
  tiledata = data;
  tile_headers.swap(headers);

  tileset = Tileset(width);
  for (int i = 0; i < tile_count; ++i)
    {
      Tile tile(TileProvider(new NetPanzerTileProviderImpl(i)));
      tileset.add_tile(i, &tile);
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
  if (!impl.get() || impl->tilemap.is_null())
    return;

  unsigned char netp_id_header[64];
  std::memset(netp_id_header, 0, sizeof(netp_id_header));
  std::strncpy(reinterpret_cast<char*>(netp_id_header), impl->id_header.c_str(),
               sizeof(netp_id_header) - 1);

  uint16_t id = 0;
  char name[256];
  char description[1024];
  std::memset(name, 0, sizeof(name));
  std::memset(description, 0, sizeof(description));
  std::strncpy(name, impl->name.c_str(), sizeof(name) - 1);
  std::strncpy(description, impl->description.c_str(), sizeof(description) - 1);

  const int width  = impl->tilemap.get_width();
  const int height = impl->tilemap.get_height();
  if (width <= 0 || height <= 0 || width > 65535 || height > 65535)
    {
      std::cout << "NetPanzerFileStruct::save: invalid dimensions" << std::endl;
      return;
    }

  uint16_t x_size = static_cast<uint16_t>(width);
  uint16_t y_size = static_cast<uint16_t>(height);
  char tile_set[256];
  std::memset(tile_set, 0, sizeof(tile_set));
  std::strncpy(tile_set, "summer12mb.tls", sizeof(tile_set) - 1);

  uint16_t thumbnail_x_pix = x_size;
  uint16_t thumbnail_y_pix = y_size;

  std::ofstream out(filename.c_str(), std::ios::binary);
  if (!out)
    {
      std::cout << "NetPanzerFileStruct::save: cannot open " << filename << std::endl;
      return;
    }

  out.write(reinterpret_cast<char*>(netp_id_header), sizeof(netp_id_header));
  write_le16(out, id);
  out.write(name, sizeof(name));
  out.write(description, sizeof(description));
  write_le16(out, x_size);
  write_le16(out, y_size);
  out.write(tile_set, sizeof(tile_set));
  write_le16(out, thumbnail_x_pix);
  write_le16(out, thumbnail_y_pix);

  Field<int>* field = impl->tilemap.get_field();
  const int cells = width * height;
  std::vector<uint16_t> vec(static_cast<std::size_t>(cells), 0);
  if (field)
    {
      for (int i = 0; i < cells; ++i)
        vec[static_cast<std::size_t>(i)] = static_cast<uint16_t>((*field)[i] & 0xffff);
    }
  write_le16_array(out, vec);

  std::vector<unsigned char> thumbnail(static_cast<std::size_t>(cells), 0);
  if (field && NetPanzerData::instance()->has_data())
    {
      for (int i = 0; i < cells; ++i)
        {
          Tile* tile = impl->tileset.create((*field)[i]);
          if (tile)
            thumbnail[static_cast<std::size_t>(i)] =
              find_nearest_color(NetPanzerData::instance()->get_palette(),
                                 tile->get_color());
        }
    }
  out.write(reinterpret_cast<char*>(&thumbnail[0]),
            static_cast<std::streamsize>(thumbnail.size()));
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

  unsigned char netp_id_header[64];
  uint16_t id = 0;
  char name[256];
  char description[1024];
  uint16_t x_size = 0;
  uint16_t y_size = 0;
  char tile_set[256];
  uint16_t thumbnail_x_pix = 0;
  uint16_t thumbnail_y_pix = 0;

  std::memset(name, 0, sizeof(name));
  std::memset(description, 0, sizeof(description));
  std::memset(tile_set, 0, sizeof(tile_set));
  std::memset(netp_id_header, 0, sizeof(netp_id_header));

  std::ifstream file(filename.c_str(), std::ios::binary);
  if (!file)
    {
      std::cout << "NetPanzerFileStruct: Error opening " << filename << std::endl;
      impl->tilemap = TilemapLayer(tileset, 1, 1);
      return;
    }

  file.read(reinterpret_cast<char*>(netp_id_header), sizeof(netp_id_header));
  id = read_le16(file);
  file.read(name, sizeof(name));
  file.read(description, sizeof(description));
  x_size = read_le16(file);
  y_size = read_le16(file);
  file.read(tile_set, sizeof(tile_set));
  thumbnail_x_pix = read_le16(file);
  thumbnail_y_pix = read_le16(file);

  if (!file || x_size == 0 || y_size == 0)
    {
      std::cout << "NetPanzerFileStruct: truncated or invalid map " << filename << std::endl;
      impl->tilemap = TilemapLayer(tileset, 1, 1);
      return;
    }

  const uint32_t cells = static_cast<uint32_t>(x_size) * static_cast<uint32_t>(y_size);
  if (cells > 4096u * 4096u)
    {
      std::cout << "NetPanzerFileStruct: map too large (" << x_size << "x" << y_size << ")" << std::endl;
      impl->tilemap = TilemapLayer(tileset, 1, 1);
      return;
    }

  TilemapLayer tilemap(tileset, static_cast<int>(x_size), static_cast<int>(y_size));
  Field<int>* field = tilemap.get_field();
  if (!field)
    {
      impl->tilemap = tilemap;
      return;
    }

  std::vector<uint16_t> vec(cells);
  read_le16_array(file, vec);
  if (!file)
    {
      std::cout << "NetPanzerFileStruct: truncated tile data in " << filename << std::endl;
    }
  else
    {
      for (uint32_t i = 0; i < cells; ++i)
        (*field)[static_cast<int>(i)] = static_cast<int>(vec[i]);
    }

  std::cout << "Thumbnail: " << thumbnail_x_pix << " " << thumbnail_y_pix << std::endl;

  impl->tilemap = tilemap;
  impl->id_header.assign(reinterpret_cast<char*>(netp_id_header),
                         strnlen(reinterpret_cast<char*>(netp_id_header), sizeof(netp_id_header)));
  name[sizeof(name) - 1] = '\0';
  description[sizeof(description) - 1] = '\0';
  impl->name = name;
  impl->description = description;
}

/* EOF */
