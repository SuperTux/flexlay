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
#include <ClanLib/gl.h>
#include "../windstille_level.hxx"
#include "../globals.hxx"
#include "tile.hxx"
#include "tile_factory.hxx"
#include "editor.hxx"
#include "editor_map.hxx"
#include "tile_brush.hxx"
#include "editor_map_component.hxx"
#include "editor_map_component.hxx"
#include "editor_tilemap.hxx"
#include "editor_map_component.hxx"

EditorTileMap::EditorTileMap(int w, int h, int tile_size_)
  : tile_size(tile_size_), field(w, h)
{
  // FIXME: Move this to the widget or to some more generic
  // map-properties thingy
  draw_grid      = true;
  draw_attribute = false;

  for (int y = 0; y < field.get_height(); ++y) 
    for (int x = 0; x < field.get_width(); ++x)
      field.at(x, y) = 0;
}

EditorTileMap::~EditorTileMap()
{
}

void
EditorTileMap::draw_tile(int id, int x, int y, bool grid, bool attribute, float alpha)
{
  Tile* tile = TileFactory::current()->create(id);

  if (tile)
    {
      CL_Sprite sprite = tile->get_sprite();
      sprite.set_alignment (origin_top_left, 0, 0);

      if (alpha != 1.0f)
        sprite.set_color(.8f, .8f, 1.0f, alpha);

      sprite.draw (x, y);
      
      if (attribute)
        CL_Display::fill_rect(CL_Rect(CL_Point(x, y), CL_Size(TILE_SIZE + 1, TILE_SIZE + 1)),
                              tile->get_attribute_color());

      if (grid)
        CL_Display::draw_rect(CL_Rect(CL_Point(x, y), CL_Size(TILE_SIZE + 1, TILE_SIZE + 1)),
                              CL_Color(128, 128, 128, 255));
    }
  else
    {
      if (grid)
        CL_Display::draw_rect (CL_Rect(CL_Point(x, y), CL_Size(TILE_SIZE + 1, TILE_SIZE + 1)),
                               CL_Color(128, 128, 128, 255));
    }
}

void
EditorTileMap::draw(EditorMapComponent* parent)
{
  CL_Display::fill_rect(CL_Rect(CL_Point(0,0),
                                CL_Size(field.get_width() * tile_size,
                                        field.get_height() * tile_size)),
                        CL_Color(0, 0, 150, 255));
  CL_Display::flush();

  // FIXME: Make layers 'transparentable'
  float alpha = 1.0f;

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
                  draw_grid, draw_attribute, alpha);
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
EditorTileMap::resize(int w, int h, int x, int y)
{
  field.resize(w, h, x, y);
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

/* EOF */
