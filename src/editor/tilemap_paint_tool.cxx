//  $Id: tilemap_paint_tool.cxx,v 1.2 2003/09/23 22:07:32 grumbel Exp $
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
#include "globals.hxx"
#include "editor_tilemap.hxx"
#include "tile_factory.hxx"
#include "editor_map.hxx"
#include "tilemap_paint_tool.hxx"

TileMapPaintTool* TileMapPaintTool::current_ = 0; 

TileMapPaintTool::TileMapPaintTool(EditorMap* p, EditorTileMap* t)
  : TileMapTool(p), 
    tilemap(t)
{
  last_draw = CL_Point(-1, -1);
  painting = false;
  opaque = false;
  current_ = this;
}

TileMapPaintTool::~TileMapPaintTool()
{
}

void
TileMapPaintTool::draw()
{
  CL_Point pos = parent->screen2tile(CL_Point(CL_Mouse::get_x(), CL_Mouse::get_y()));

  if (0)
    {
      Tile* tile = TileFactory::current()->create(tilemap->brush_tile);

      if (tile)
        {
          CL_Sprite sprite = tile->sur;
          sprite.set_alpha(0.5f);
          sprite.draw(pos.x * TILE_SIZE, pos.y * TILE_SIZE);
        }

      CL_Display::fill_rect (CL_Rect(CL_Point(pos.x * TILE_SIZE, pos.y * TILE_SIZE),
                                     CL_Size(TILE_SIZE, TILE_SIZE)),
                             CL_Color(255, 255, 255, 100));
    }
  else 
    {
      for(int y = 0; y < brush.get_height(); ++y)
        for(int x = 0; x < brush.get_width(); ++x)
          {
            Tile* tile = TileFactory::current()->create(brush(x, y));
                
            if (tile)
              {
                CL_Sprite sprite = tile->sur;
                sprite.set_alpha(0.5f);
                sprite.draw((pos.x + x) * TILE_SIZE, 
                            (pos.y + y) * TILE_SIZE);
              }
                
            CL_Display::fill_rect (CL_Rect(CL_Point((pos.x + x) * TILE_SIZE, 
                                                    (pos.y + y) * TILE_SIZE),
                                           CL_Size(TILE_SIZE, TILE_SIZE)),
                                   CL_Color(255, 255, 255, 100));
          }
    }
}

void
TileMapPaintTool::on_mouse_down(const CL_InputEvent& event)
{
  CL_Point pos = parent->screen2tile(event.mouse_pos);

  if (event.id == CL_MOUSE_LEFT)
    { 
      tilemap->draw_tile(brush, parent->screen2tile(event.mouse_pos), opaque);
      last_draw = pos;

      painting = true;
    }
  else if (event.id == CL_MOUSE_RIGHT)
    {
      tilemap->brush_tile = tilemap->get_field()->at(pos.x, pos.y)->get_id();
    }
}
 
void
TileMapPaintTool::on_mouse_move(const CL_InputEvent& event)
{
  if (painting)
    {
      CL_Point pos = parent->screen2tile(event.mouse_pos);
      if (pos != last_draw)
        {
          tilemap->draw_tile(brush, pos, opaque);
          last_draw = pos;
        }
    }
}

void
TileMapPaintTool::on_mouse_up  (const CL_InputEvent& event)
{
  if (event.id == CL_MOUSE_LEFT)
    {
      tilemap->draw_tile(brush, parent->screen2tile(event.mouse_pos), opaque);
      last_draw = CL_Point(-1, -1);

      painting = false;
    }
  else if (event.id == CL_MOUSE_RIGHT)
    {
    }
}

void
TileMapPaintTool::set_brush(const TileBrush& b)
{
  brush = b;
}

/* EOF */
