//  $Id: tilemap_diamond_tool.cxx,v 1.2 2003/09/26 14:29:36 grumbel Exp $
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
#include <ClanLib/Display/input_event.h>
#include "editor_tilemap.hxx"
#include "globals.hxx"
#include "tilemap_diamond_tool.hxx"

TileMapDiamondTool::TileMapDiamondTool(EditorTileMap* t)
  : TileMapTool(t),
    sprite("diamond", resources)
{
  painting = false;
}

TileMapDiamondTool::~TileMapDiamondTool() 
{
}

void
TileMapDiamondTool::draw()
{
  CL_Point pos = tilemap->screen2world(CL_Point(CL_Mouse::get_x(), CL_Mouse::get_y()));
 
  sprite.set_frame(7);
  sprite.draw(pos.x/64 * 64,
              pos.y/64 * 64);
}

void
TileMapDiamondTool::on_mouse_up  (const CL_InputEvent& event)
{
  painting = false;
  paint(event.mouse_pos);
}

void
TileMapDiamondTool::on_mouse_down(const CL_InputEvent& event)
{
  if (event.id == CL_MOUSE_LEFT)
    {
      color = true;
    }
  else
    {
      color = false;
    }

  painting = true;
  paint(event.mouse_pos);
}

void
TileMapDiamondTool::on_mouse_move(const CL_InputEvent& event)
{
  if (painting)
    {
      paint(event.mouse_pos);
    }
}

void
TileMapDiamondTool::paint(const CL_Point& mpos)
{
  CL_Point pos = tilemap->screen2world(mpos);

  tilemap->get_diamond_map()->at(int(pos.x/64), 
                                 int(pos.y/64)) = color;
}

/* EOF */
