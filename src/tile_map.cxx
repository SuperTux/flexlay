//  $Id: tile_map.cxx,v 1.4 2003/08/11 11:18:11 grumbel Exp $
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
#include "tile.hxx"
#include "globals.hxx"

extern CL_ResourceManager* resources;

TileMap::TileMap (WindstilleLevel* data)
  : field (data->get_field()->get_width (),
	   data->get_field()->get_height ())
{
  std::cout << "Creating Tilemap: " 
            << data->get_field()->get_width() << "x" << data->get_field()->get_height() << std::endl;

  for (unsigned int y = 0; y < field.get_height (); ++y) {
    for (unsigned int x = 0; x < field.get_width (); ++x)
      {
        unsigned char cole[8] = { 0, 0, 0, 0, 0, 0, 0, 0 };
        unsigned char colf[8] = { 255, 255, 255, 255, 255, 255, 255, 255 };
        unsigned char* col = colf;

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
            name == "none")
          {
            col = cole;
          }

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
            for(int tile_y = 0; tile_y < 8; ++tile_y)
              for(int tile_x = 0; tile_x < 8; ++tile_x)
                {
                  if (field (x,y)->get_col(tile_x, tile_y))
                    {
                      CL_Display::fill_rect(CL_Rect(x * TILE_SIZE + tile_x*16, y*TILE_SIZE + tile_y*16,
                                                    x * TILE_SIZE + tile_x*16 + 16, y * TILE_SIZE + tile_y*16 + 16),
                                            CL_Color(255, 0, 0, 128));
                    }
                }
	  }
      }
}

bool
TileMap::is_ground (float x, float y)
{
  unsigned int x_pos = int(x) / TILE_SIZE;
  unsigned int y_pos = int(y) / TILE_SIZE;

  unsigned int sub_tile_x = int(x) / (TILE_SIZE/8) - x_pos*8;
  unsigned int sub_tile_y = int(y) / (TILE_SIZE/8) - y_pos*8;

  if (x_pos < 0 || x_pos >= field.get_width () || y_pos < 0 || y_pos >= field.get_height ())
    {
      std::cout << "TileMap::is_ground (): Out of range: " << x_pos << " " << y_pos << std::endl;
      return 1;
    }

  if (field (x_pos, y_pos))
    return field (x_pos, y_pos)->get_col(sub_tile_x, sub_tile_y);
  else
    return 0;
}

/* EOF */
