//  $Id: tilemap_paint_tool.cxx,v 1.1 2003/09/23 19:10:05 grumbel Exp $
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
#include "editor_tilemap.hxx"
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
}

void
TileMapPaintTool::on_mouse_down(const CL_InputEvent& event)
{
  CL_Point pos = tilemap->screen2tile(event.mouse_pos);
  
  if (pos.x >= 0 && pos.x < tilemap->get_field()->get_width()
      && pos.y >= 0 && pos.y < tilemap->get_field()->get_height())
    tilemap->get_field()->at(pos.x, pos.y)->set_tile(tilemap->brush_tile);

  painting = true;
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
  painting = false;
}

/* EOF */
