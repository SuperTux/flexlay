//  $Id: tile_map.cxx,v 1.2 2003/08/11 08:03:23 grumbel Exp $
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

#include <ClanLib/gl.h>
#include <sstream>
#include "tile_map.hxx"
#include "windstille_level.hxx"
#include "globals.hxx"

extern CL_ResourceManager* resources;

Tile::Tile (CL_Sprite arg_sur, int col)
: sur (arg_sur),
  collision (col)
{
}

TileMap::TileMap (WindstilleLevel* data)
  : field (data->get_field()->get_width (),
	   data->get_field()->get_height ())
{
  std::cout << "Creating Tilemap: " 
            << data->get_field()->get_width() << "x" << data->get_field()->get_height() << std::endl;

  for (unsigned int y = 0; y < field.get_height (); ++y) {
    for (unsigned int x = 0; x < field.get_width (); ++x)
      {
	int col = 1;
	std::string name = (*data->get_field()) (x, y);
	if (name == "tiles/green1" ||
            name == "tiles/tile3"  ||
            name == "tiles/tile2"  ||
            name == "tiles/tile4"  ||
	    name == "tiles/tile10" ||
	    name == "tiles/tile15" ||
	    name == "tiles/tile16" ||
	    name == "tiles/tile6" ||
	    name == "tiles/tile7" ||
	    name == "tiles/tile8" ||
	    name == "tiles/tile64" ||
	    name == "tiles/tile65" ||
	    name == "tiles/tile66" ||
	    name == "tiles/tile67" ||
	    name == "tiles/tile68" ||
	    name == "tiles/tile69" ||
	    name == "tiles/tile70" ||
	    name == "tiles/tile71" ||
	    name == "tiles/tile72" ||
	    name == "tiles/tile73" ||
	    name == "tiles/tile74" ||
	    name == "tiles/tile75" ||
	    name == "tiles/tile76" ||
	    name == "tiles/tile77" ||
            name == "none"
	    )
	  col = 0;
	std::cout << col;

	if (name != "none")
	  field (x, y) = new Tile (CL_Sprite(name, resources), col);
	else
	  field (x, y) = 0;
      }
    std::cout << std::endl;
  }
}

void 
TileMap::update (float delta)
{
  /*for (FieldIter i = field.begin (); i != field.end (); ++i)
    {
      (*i)->update (delta);
      }*/
}

void
TileMap::draw ()
{
  for (unsigned int y = 0; y < field.get_height (); ++y)
    for (unsigned int x = 0; x < field.get_width (); ++x)
      {
	//field (x,y)->sur->setScale (2.0f, 2.0f);
	if (field (x,y))
	  {
	    field (x,y)->sur.draw (x * TILE_SIZE + TILE_SIZE/2, y * TILE_SIZE + TILE_SIZE/2);
	    if (field (x,y)->collision)
	      {
		//CL_Display::fill_rect (x * TILE_SIZE, y*TILE_SIZE, x*TILE_SIZE + TILE_SIZE, y*TILE_SIZE + TILE_SIZE, 1.0, 1.0, 1.0, .5);
	      }
	  }
      }
}

bool
TileMap::is_ground (float x, float y)
{
  unsigned int x_pos = int(x) / TILE_SIZE;
  unsigned int y_pos = int(y) / TILE_SIZE;

  if (x_pos < 0 || x_pos >= field.get_width () || y_pos < 0 || y_pos >= field.get_height ())
    {
      std::cout << "TileMap::is_ground (): Out of range: " << x_pos << " " << y_pos << std::endl;
      return 1;
    }

  if (field (x_pos, y_pos))
    return field (x_pos, y_pos)->collision;
  else
    return 0;
}

/* EOF */
