//  $Id: editor_tile.cxx,v 1.5 2003/09/10 13:53:11 grumbel Exp $
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

#include <assert.h>
#include <string>
#include "../globals.hxx"
#include "../tile_factory.hxx"
#include "editor_tile.hxx"

EditorTile::EditorTile ()
{
  set_tile (0);
}

EditorTile::EditorTile (int id)
{
  tile = TileFactory::current()->create(id);
}

void
EditorTile::draw (int x, int y)
{
  if (tile)
    {
      tile->sur.set_alignment (origin_top_left, 0, 0);
      tile->sur.draw (x, y);
      CL_Display::draw_rect(CL_Rect(x, y, x + TILE_SIZE, y + TILE_SIZE),
                            CL_Color(255, 255, 255, 128));
    }
  else
    {
      CL_Display::fill_rect (CL_Rect(x, y, x + TILE_SIZE, y + TILE_SIZE),
			     CL_Color(77, 77, 77, 255));
      CL_Display::draw_rect (CL_Rect(x, y, x + TILE_SIZE, y + TILE_SIZE),
			     CL_Color(255, 255, 255, 128));
    }
}

void
EditorTile::set_tile (int arg_id)
{
  id = arg_id;
  tile = TileFactory::current()->create(id);
}

/* EOF */
