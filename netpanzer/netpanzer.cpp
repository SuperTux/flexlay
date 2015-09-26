//  Flexlay - A Generic 2D Game Editor
//  Copyright (C) 2002 Ingo Ruhnke <grumbel@gmail.com>
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
#include <sstream>
#include <fstream>

#include "blitter.hpp"
#include "editor_map.hpp"
#include "globals.hpp"
#include "netpanzer.hpp"
#include "tile.hpp"
#include "tile_provider.hpp"
#include "tile_provider_impl.hpp"
#include "tilemap_layer.hpp"
#include "tileset.hpp"

NetPanzerData* NetPanzerData::instance_ = 0;

class NetPanzerTileProviderImpl : public TileProviderImpl
{
private:
  int id;
  mutable Sprite sprite;
  mutable PixelBuffer buffer;

public:
  NetPanzerTileProviderImpl(int id_)
    : id(id_)
  {
  }

  virtual ~NetPanzerTileProviderImpl()
  {
  }

  Sprite get_sprite() const
  {
    if (sprite)
    {
      return sprite;
    }
    else
    {
      NetPanzerTileGroup& tilegroup = NetPanzerData::instance()->find_tilegroup(id);
      int dist = id - tilegroup.start;

      Rect rect(Point((dist % tilegroup.width) * 32,
                      (dist / tilegroup.width) * 32),
                Size(32, 32));
      sprite.add_frame(tilegroup.get_surface(), rect);
      return sprite;
    }
  }

  PixelBuffer get_pixelbuffer() const
  {
    if (buffer)
    {
      return buffer;
    }
    else
    {
      const Palette& palette = NetPanzerData::instance()->get_palette();
      unsigned char* data = NetPanzerData::instance()->get_tiledata() + (32*32) * id;
      buffer = PixelBuffer(32, 32);

      buffer.lock();
      unsigned char* target = static_cast<unsigned char*>(buffer.get_data());

      for(int i = 0; i < 32*32; ++i)
      {
        target[4*i+3] = palette[data[i]].get_blue();
        target[4*i+2] = palette[data[i]].get_green();
        target[4*i+1] = palette[data[i]].get_red();
        target[4*i+0] = 255;
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
  load_tileset(datadir + "/" + "wads/summer12mb.tls");
}

Surface
NetPanzerTileGroup::get_surface()
{
  if (!surface)
  {
    PixelBuffer buffer(width*32, height*32);

    for(int y = 0; y < height; ++y)
      for(int x = 0; x < width; ++x)
      {
        const Palette& palette = NetPanzerData::instance()->get_palette();
        unsigned char* data = NetPanzerData::instance()->get_tiledata() + (32*32) * (start + width*y + x);

        NetPanzerTileHeader header = NetPanzerData::instance()->get_tile_headers()[start + width*y + x];

        PixelBuffer tile(32, 32);

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
          target[4*i+3] = int(palette[data[i]].get_blue() * b);
          target[4*i+2] = int(palette[data[i]].get_green() * g);
          target[4*i+1] = int(palette[data[i]].get_red() * r);
          target[4*i+0] = 255;
        }
        tile.unlock();

        blit(buffer, tile, x * 32, y * 32);
      }

    surface = Surface(buffer);
  }

  return surface;
}

Sprite
NetPanzerData::get_tilegroup_sprite(int index)
{
  for(TileGroups::iterator i = tilegroups.begin(); i != tilegroups.end(); ++i)
  {
    if (index == i->start)
    {
      Sprite sprite;
      sprite.add_frame(i->get_surface(), Rect(Point(0, 0),
                                              Size(i->get_surface().get_width(),
                                                   i->get_surface().get_height())));
      return sprite;
    }
  }

  std::cout << "NetPanzerData: Couldn't get tilegroup_sprite for '" << index << "'" << std::endl;
  return Sprite();
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

const Palette&
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

Palette
NetPanzerData::load_palette(const std::string& filename)
{
  Palette palette;
  unsigned char color_array[256 * 3];

  std::ifstream in(filename.c_str());

  if (!in)
  {
    std::cout << "Couldn't load palette" << std::endl;
    return palette;
  }
  else
  {
    in.read(reinterpret_cast<char*>(color_array), sizeof(color_array));

    for(int i = 0; i < 256; ++i)
    {
      palette.colors[i].set_red(color_array[3*i + 0]);
      palette.colors[i].set_green(color_array[3*i + 1]);
      palette.colors[i].set_blue(color_array[3*i + 2]);
    }

    return palette;
  }
}

void
NetPanzerData::load_tileset(const std::string& filename)
{
  unsigned char netp_id_header[64];
  unsigned short version;
  unsigned short width;
  unsigned short height;
  unsigned short tile_count;
  unsigned char raw_palette[768];

  std::ifstream file(filename.c_str());

  if (!file)
  {
    std::cout << "Couldn't load " << filename << std::endl;
  }
  else
  {
    file.read(reinterpret_cast<char*>(netp_id_header), sizeof(netp_id_header));
    file.read(reinterpret_cast<char*>(&version), sizeof(version));
    file.read(reinterpret_cast<char*>(&width), sizeof(width));
    file.read(reinterpret_cast<char*>(&height), sizeof(height));
    file.read(reinterpret_cast<char*>(&tile_count), sizeof(tile_count));
    file.read(reinterpret_cast<char*>(raw_palette), sizeof(raw_palette));

    tile_headers.resize(tile_count);

    file.read(reinterpret_cast<char*>(&*tile_headers.begin()),
              sizeof(NetPanzerTileHeader)*tile_count);

    uint32_t tilesize = width * height;
    // FIXME: Delete this somewhere!
    unsigned char* tiledata = new unsigned char[tilesize*tile_count];
    file.read(reinterpret_cast<char*>(tiledata), tilesize*tile_count);
    file.close();

    // FIXME: The palette in the netpanzer 'summer12mb.tls' file
    // is either broken or otherwise corrupt, so we ignore it
    // and use the seperate palette file 'netp.act' which works
    // fine.

    NetPanzerData::instance()->tiledata = tiledata;

    tileset = Tileset(width);

    for(int i = 0; i < tile_count; ++i)
    {
      Tile tile(TileProvider(new NetPanzerTileProviderImpl(i)));
      tileset.add_tile(i, &tile);
    }
  }
}

unsigned char find_nearest_color(const Palette& palette, const Color& rgb)
{
  // Copyright (C) 1998 Pyrosoft Inc. (www.pyrosoftgames.com), Matthew Bogue
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

  int tile_count = impl->tilemap.get_tileset().get_tiles().size();
  Field<int>* field = impl->tilemap.get_field();
  for(int i = 0; i < x_size * y_size; ++i)
  {
    // Fill everything that isn't a valid tile with grass
    if ((*field)[i] >= 0 && (*field)[i] < tile_count)
      vec[i] = (*field)[i];
    else
      vec[i] = 8097 + rand()%16;
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

NetPanzerFileStruct::NetPanzerFileStruct(Tileset tileset, const std::string& filename) :
  impl(new NetPanzerFileStructImpl())
{
  impl->tileset = tileset;

  // FIXME: endian issues
  unsigned char   netp_id_header[64]; // Copyright PyroSoft Inc....
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
