//  $Id: editor_tile.cxx,v 1.7 2003/09/11 18:58:19 grumbel Exp $
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

void
EditorTile::draw(int id, int x, int y, bool grid, bool attribute, float alpha)
{
  // FIXME: Slow due to vector vs map
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
                              tile->get_color());

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

/* EOF */
