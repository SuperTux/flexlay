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
#include "tilemap_paint_tool.hxx"

TileMapPaintTool::TileMapPaintTool(EditorTileMap* t)
  : TileMapTool(t)
{
  painting = false;
}

TileMapPaintTool::~TileMapPaintTool()
{
}

void
TileMapPaintTool::draw()
{
  CL_Point pos = tilemap->screen2tile(CL_Point(CL_Mouse::get_x(), CL_Mouse::get_y()));

  if (pos.x >= 0 && pos.y >= 0)
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
}

void
TileMapPaintTool::on_mouse_down(const CL_InputEvent& event)
{
  CL_Point pos = tilemap->screen2tile(event.mouse_pos);

  if (event.id == CL_MOUSE_LEFT)
    { 
      if (pos.x >= 0 && pos.x < tilemap->get_field()->get_width()
          && pos.y >= 0 && pos.y < tilemap->get_field()->get_height())
        tilemap->get_field()->at(pos.x, pos.y)->set_tile(tilemap->brush_tile);

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
      CL_Point pos = tilemap->screen2tile(event.mouse_pos);
      if (pos.x >= 0 && pos.x < tilemap->get_field()->get_width()
          && pos.y >= 0 && pos.y < tilemap->get_field()->get_height())
        tilemap->get_field()->at(pos.x, pos.y)->set_tile(tilemap->brush_tile);
    }
}

void
TileMapPaintTool::on_mouse_up  (const CL_InputEvent& event)
{
  if (event.id == CL_MOUSE_LEFT)
    {
      painting = false;
    }
  else if (event.id == CL_MOUSE_RIGHT)
    {
    }
}

/* EOF */
