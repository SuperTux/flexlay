//  $Id: tile_map.cxx,v 1.18 2003/11/05 11:09:36 grumbel Exp $
//
//  Windstille - A Jump'n Shoot Game
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
#include "tile_factory.hxx"
#include "globals.hxx"
#include "view.hxx"

extern CL_ResourceManager* resources;

TileMap::TileMap(int w, int h)
  : field(w, h)
{
}

TileMap::TileMap (Field<int>* data)
  : field(data->get_width(),
          data->get_height())
{
  if (debug)
    {
      std::cout << "TileMap: Size: " 
                << data->get_width() << "x" << data->get_height() << std::endl;
    }

  for (int y = 0; y < field.get_height (); ++y) 
    {
      for (int x = 0; x < field.get_width (); ++x)
        {
          field(x, y) = TileFactory::current()->create((*data)(x, y));
        }
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
  CL_Rect rect = View::current()->get_clip_rect();

  int start_x = std::max(0, rect.left/TILE_SIZE);
  int start_y = std::max(0, rect.top/TILE_SIZE);
  int end_x   = std::min(field.get_width(),  rect.right/TILE_SIZE + 1);
  int end_y   = std::min(field.get_height(), rect.bottom/TILE_SIZE + 1);

  for (int y = start_y;   y < end_y; ++y)
    for (int x = start_x; x < end_x; ++x)
      {
	//field (x,y)->sur->setScale (2.0f, 2.0f);
	if (field (x,y))
	  {
	    field (x,y)->get_sprite().draw (x * TILE_SIZE, 
                                            y * TILE_SIZE);
            if (1) // Draw subtiles
              {
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
}

bool
TileMap::get_pixel(int x, int y)
{
  if (x < 0 || y < 0 
      || x >= int(field.get_width()*SUBTILE_NUM)
      || y >= int(field.get_height()*SUBTILE_NUM))
    {
      //std::cout << "Out of bounce: " << x << ", " << y << std::endl;
      return 0;
    }
  else
    {
      Tile* tile = field(x / SUBTILE_NUM,
                         y / SUBTILE_NUM);
      if (0)
        std::cout << "GetPixel: " << x / SUBTILE_NUM << "," << x % SUBTILE_NUM << " x "
                << y / SUBTILE_NUM << "," << y % SUBTILE_NUM << std::endl;

      if (tile)
        return tile->get_col(x % SUBTILE_NUM,
                             y % SUBTILE_NUM);
      else
        return 0;     
    }
}

bool
TileMap::is_ground (float x, float y)
{
  int x_pos = int(x) / TILE_SIZE;
  int y_pos = int(y) / TILE_SIZE;

  int sub_tile_x = int(x) / (TILE_SIZE/8) - x_pos*8;
  int sub_tile_y = int(y) / (TILE_SIZE/8) - y_pos*8;

  if (x < 0 || x_pos >= field.get_width())
    {
      //std::cout << "TileMap::is_ground (): Out of range: " << x_pos << " " << y_pos << std::endl;
      return 1;
    }
  else if (y < 0 || y_pos >= field.get_height())
    {
      return 0;
    }


  if (field (x_pos, y_pos))
    return field (x_pos, y_pos)->get_col(sub_tile_x, sub_tile_y);
  else
    return 0;
}

/* EOF */
