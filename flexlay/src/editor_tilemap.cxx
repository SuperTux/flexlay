//  $Id: editor_tilemap.cxx,v 1.14 2003/09/26 14:29:36 grumbel Exp $
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

#include <math.h>
#include <iostream>
#include <ClanLib/Display/display.h>
#include <ClanLib/Display/pixel_buffer.h>
#include <ClanLib/Display/pixel_format.h>
#include <ClanLib/Display/palette.h>
#include <ClanLib/gl.h>
#include "tile.hxx"
#include "tileset.hxx"
#include "editor.hxx"
#include "editor_map.hxx"
#include "tile_brush.hxx"
#include "editor_map_component.hxx"
#include "editor_map_component.hxx"
#include "editor_tilemap.hxx"
#include "editor_map_component.hxx"

EditorTileMap* EditorTileMap::current_ = 0;

EditorTileMap::EditorTileMap(Tileset* tileset_, int w, int h, int tile_size_)
  : tile_size(tile_size_), field(w, h)
{
  // FIXME: Move this to the widget or to some more generic
  // map-properties thingy
  draw_grid      = false;
  draw_attribute = false;
  hex_mode = false;

  for (int y = 0; y < field.get_height(); ++y) 
    for (int x = 0; x < field.get_width(); ++x)
      field.at(x, y) = 0;

  background_color = CL_Color(0, 0, 0, 0);
  foreground_color = CL_Color(255, 255, 255, 255);
  
  if (!tileset_)
    tileset = Tileset::current();
  else
    tileset = tileset_;
}

EditorTileMap::~EditorTileMap()
{
}

void
EditorTileMap::draw_tile(int id, int x, int y, bool attribute)
{
  Tile* tile = tileset->create(id);

  if (tile)
    {
      CL_Sprite sprite = tile->get_sprite();
      sprite.set_alignment (origin_top_left, 0, 0);

      sprite.set_color(foreground_color);

      sprite.draw (x, y);
      
      if (attribute)
        CL_Display::fill_rect(CL_Rect(CL_Point(x, y), CL_Size(tile_size, tile_size)),
                              tile->get_attribute_color());
    }
}

void
EditorTileMap::draw(EditorMapComponent* parent)
{
  if (background_color.get_alpha() != 0)
    CL_Display::fill_rect(CL_Rect(CL_Point(0,0),
                                  CL_Size(field.get_width() * tile_size,
                                          field.get_height() * tile_size)),
                          background_color);
  CL_Display::flush();

  CL_Rect rect = parent->get_clip_rect();

  int start_x = std::max(0, rect.left/tile_size);
  int start_y = std::max(0, rect.top/tile_size);
  int end_x   = std::min(field.get_width(),  rect.right/tile_size + 1);
  int end_y   = std::min(field.get_height(), rect.bottom/tile_size + 1);

  for (int y = start_y; y < end_y; ++y)
    for (int x = start_x; x < end_x; ++x)
      {
        draw_tile(field.at(x, y), 
                  x * tile_size, y * tile_size, 
                  draw_attribute);
      }

  if (draw_grid)
    {
      for (int y = start_y; y <= end_y; ++y)
        CL_Display::draw_line(start_x * tile_size,
                              y * tile_size,
                              end_x   * tile_size,
                              y * tile_size, 
                              y % 5 ?
                              CL_Color(150, 150, 150) :
                              CL_Color(255, 255, 255)
                              );
  
      for (int x = start_x; x <= end_x; ++x)
        CL_Display::draw_line(x * tile_size,
                              start_y * tile_size,
                              x   * tile_size,
                              end_y * tile_size, 
                              x % 5 ?
                              CL_Color(150, 150, 150) :
                              CL_Color(255, 255, 255));
    }

  CL_Display::flush();
}

int
EditorTileMap::get_tile (int x, int y)
{
  if (x >= 0 && x < (int)field.get_width() &&
      y >= 0 && y < (int)field.get_height())
    return field.at(x, y);
  else
    return 0;
}

void
EditorTileMap::resize(const CL_Size& size, const CL_Point& point)
{
  field.resize(size.width, size.height, point.x, point.y);
}

void
EditorTileMap::draw_tile(int id, const CL_Point& pos)
{
  if (pos.x >= 0 && pos.x < field.get_width()
      && pos.y >= 0 && pos.y < field.get_height())
    {
      field.at(pos.x, pos.y) = id;
    }
}

void
EditorTileMap::draw_tile(const TileBrush& brush, const CL_Point& pos)
{
  draw_tile(&field, brush, pos);
}

void
EditorTileMap::draw_tile(Field<int>* field, const TileBrush& brush, const CL_Point& pos)
{
  int start_x = std::max(0, -pos.x);
  int start_y = std::max(0, -pos.y);

  int end_x = std::min(brush.get_width(),  field->get_width()  - pos.x);
  int end_y = std::min(brush.get_height(), field->get_height() - pos.y);

  for (int y = start_y; y < end_y; ++y)
    for (int x = start_x; x < end_x; ++x)
      {
        if (brush.is_opaque() || brush.at(x, y) != 0)
          {
            field->at(pos.x + x, pos.y + y) = brush.at(x, y);
          }
      }  
}

void
EditorTileMap::set_draw_attribute(bool t)
{
  draw_attribute = t;
}

bool
EditorTileMap::get_draw_attribute() const
{
  return draw_attribute;
}

void
EditorTileMap::set_draw_grid(bool t)
{
  draw_grid = t;
}

bool
EditorTileMap::get_draw_grid() const
{
  return draw_grid;
}

void 
blit(CL_PixelBuffer& target, CL_PixelBuffer& brush, int x_pos, int y_pos)
{
  target.lock();
  brush.lock();

  int start_x = std::max(0, -x_pos);
  int start_y = std::max(0, -y_pos);
  
  int end_x = std::min(brush.get_width(),  target.get_width()  - x_pos);
  int end_y = std::min(brush.get_height(), target.get_height() - y_pos);

  unsigned char* target_buf = static_cast<unsigned char*>(target.get_data());
  unsigned char* brush_buf  = static_cast<unsigned char*>(brush.get_data());

  int target_width = target.get_width();
  int brush_width  = brush.get_width();

  if (brush.get_format().get_type() == pixelformat_rgba)
    {
      if (brush.get_format().get_depth() == 32)
        {
          for (int y = start_y; y < end_y; ++y)
            for (int x = start_x; x < end_x; ++x)
              {
                int target_pos = (y + y_pos) * target_width + x + x_pos;
                int brush_pos  = y * brush_width + x;

                unsigned char a  = brush_buf[4*brush_pos + 0];
                unsigned char r  = brush_buf[4*brush_pos + 1];
                unsigned char g  = brush_buf[4*brush_pos + 2];
                unsigned char b  = brush_buf[4*brush_pos + 3];

                unsigned char ta = target_buf[4*target_pos + 0];
                unsigned char tr = target_buf[4*target_pos + 1];
                unsigned char tg = target_buf[4*target_pos + 2];
                unsigned char tb = target_buf[4*target_pos + 3];

                float alpha  = a/255.0f;
        
                target_buf[4*target_pos + 0] = std::min(255, ta + a);
                target_buf[4*target_pos + 1] = std::min(255, int((1-alpha)*tr + alpha*r));
                target_buf[4*target_pos + 2] = std::min(255, int((1-alpha)*tg + alpha*g));
                target_buf[4*target_pos + 3] = std::min(255, int((1-alpha)*tb + alpha*b));
              }
        }
      else if (brush.get_format().get_depth() == 24)
        {
          for (int y = start_y; y < end_y; ++y)
            for (int x = start_x; x < end_x; ++x)
              {
                int target_pos = (y + y_pos) * target_width + x + x_pos;
                int brush_pos  = y * brush_width + x;

                target_buf[4*target_pos + 0] = 255;
                target_buf[4*target_pos + 1] = brush_buf[3*brush_pos + 0];
                target_buf[4*target_pos + 2] = brush_buf[3*brush_pos + 1];
                target_buf[4*target_pos + 3] = brush_buf[3*brush_pos + 2];
              }
        }
      else
        {
          std::cout << "Unsupported bpp: " << brush.get_format().get_depth() << std::endl;
        }
    }
  else if (brush.get_format().get_type() == pixelformat_index)
    {
      CL_Palette palette = brush.get_palette();
      for (int y = start_y; y < end_y; ++y)
        for (int x = start_x; x < end_x; ++x)
          {
            int target_pos = (y + y_pos) * target_width + x + x_pos;
            int brush_pos  = y * brush_width + x;
            
            target_buf[4*target_pos + 0] = 255;
            target_buf[4*target_pos + 1] = palette.colors[brush_buf[brush_pos]].get_blue();
            target_buf[4*target_pos + 2] = palette.colors[brush_buf[brush_pos]].get_green();
            target_buf[4*target_pos + 3] = palette.colors[brush_buf[brush_pos]].get_red();
          }
    }
  else
    {
      assert(!"Unknown pixelformat type");
    }
    


  brush.unlock();
  target.unlock();
}

CL_PixelBuffer*
EditorTileMap::create_pixelbuffer()
{
  CL_PixelBuffer* pixelbuffer = new CL_PixelBuffer(get_width()  * tile_size,
                                                   get_height() * tile_size,
                                                   get_width()  * tile_size * 4,
                                                   CL_PixelFormat::rgba8888);

  {
    pixelbuffer->lock();
    unsigned char* buf = static_cast<unsigned char*>(pixelbuffer->get_data());

    int width  = pixelbuffer->get_width();
    int height = pixelbuffer->get_height();

    // Draw a nice gradient
    for(int y = 0; y < height; ++y)
      {
        for (int x = 0; x < width; ++x)
          {
            buf[4*(y*width + x) + 0] = 255;
            buf[4*(y*width + x) + 1] = 255;
            buf[4*(y*width + x) + 2] = 255*y/height;
            buf[4*(y*width + x) + 3] = 255*y/height;
          }
      }
    pixelbuffer->unlock();
  }

  for (int y = 0; y < get_height(); ++y)
    for (int x = 0; x < get_width(); ++x)
      {
        Tile* tile = tileset->create(field.at(x, y));

        if (tile)
          {
            CL_PixelBuffer* buf = tile->get_pixelbuffer();
            if (buf)
              {
                blit(*pixelbuffer, *buf, x*tile_size, y*tile_size);
              }
          }
      }

  return pixelbuffer;
}

CL_Rect
EditorTileMap::get_bounding_rect()
{
  return CL_Rect(CL_Point(0, 0),
                 CL_Size(field.get_width() * tile_size, field.get_height() * tile_size));
}

CL_Point
EditorTileMap::world2tile(const CL_Point& pos) const
{
  int x = pos.x/tile_size;
  int y = pos.y/tile_size;

  return CL_Point(pos.x < 0 ? x-1 : x,
                  pos.y < 0 ? y-1 : y);
}

/* EOF */
