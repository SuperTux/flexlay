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
#include <ClanLib/Display/mouse.h>
#include <ClanLib/Display/keys.h>
#include <ClanLib/Display/display.h>
#include "globals.hxx"
#include "editor_tilemap.hxx"
#include "tile_factory.hxx"
#include "editor_map.hxx"
#include "../tile.hxx"
#include "tilemap_paint_tool.hxx"

TileMapPaintTool* TileMapPaintTool::current_ = 0; 

TileMapPaintTool::TileMapPaintTool(EditorMap* p, EditorTileMap* t)
  : TileMapTool(p), 
    tilemap(t)
{
  last_draw = CL_Point(-1, -1);
  painting  = false;
  current_  = this;
  brush = TileBrush(1, 1);
  brush.at(0, 0) = 0;
  brush.set_opaque();
  current_tile = CL_Point(0,0);
}

TileMapPaintTool::~TileMapPaintTool()
{
}

void
TileMapPaintTool::draw()
{
  // FIXME: Move ths to editor tile
  for(int y = 0; y < brush.get_height(); ++y)
    for(int x = 0; x < brush.get_width(); ++x)
      {
        Tile* tile = TileFactory::current()->create(brush(x, y));
                
        if (tile)
          {
            CL_Sprite sprite = tile->sur;
            sprite.set_alpha(0.5f);
            sprite.draw((current_tile.x + x) * TILE_SIZE, 
                        (current_tile.y + y) * TILE_SIZE);
          }
                
        CL_Display::fill_rect (CL_Rect(CL_Point((current_tile.x + x) * TILE_SIZE, 
                                                (current_tile.y + y) * TILE_SIZE),
                                       CL_Size(TILE_SIZE, TILE_SIZE)),
                               CL_Color(255, 255, 255, 100));
      }
}

void
TileMapPaintTool::on_mouse_down(const CL_InputEvent& event)
{
  CL_Point pos = parent->screen2tile(event.mouse_pos);

  if (event.id == CL_MOUSE_LEFT)
    { 
      tilemap->draw_tile(brush, parent->screen2tile(event.mouse_pos));
      last_draw = pos;

      painting = true;
      parent->capture_mouse();
    }
  else if (event.id == CL_MOUSE_RIGHT)
    {
      // FIXME: add support for larger brushes here (selecton like)
      brush = TileBrush(1, 1);
      brush.at(0, 0) = tilemap->get_field()->at(pos.x, pos.y);
      brush.set_opaque();
    }
}
 
void
TileMapPaintTool::on_mouse_move(const CL_InputEvent& event)
{
  current_tile = parent->screen2tile(event.mouse_pos);

  if (painting)
    {
      if (current_tile != last_draw)
        {
          tilemap->draw_tile(brush, current_tile);
          last_draw = current_tile;
        }
    }
}

void
TileMapPaintTool::on_mouse_up  (const CL_InputEvent& event)
{
  if (event.id == CL_MOUSE_LEFT)
    {
      tilemap->draw_tile(brush, parent->screen2tile(event.mouse_pos));
      last_draw = CL_Point(-1, -1);

      painting = false;
      parent->release_mouse();
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
