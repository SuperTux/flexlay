//  $Id: tile_factory.cxx,v 1.10 2003/09/22 18:37:05 grumbel Exp $
//
//  Flexlay - A Generic 2D Game Editor
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

#include <string>
#include <ClanLib/Core/System/system.h>
#include <assert.h>
#include <iostream>
#include "globals.hxx"
#include "string_converter.hxx"
#include "scm_helper.hxx"
#include "tile.hxx"
#include "tileset.hxx"

extern CL_ResourceManager* resources;

Tileset* Tileset::current_ = 0;

Tileset::Tileset(int tile_size_)
{
  current_ = this;
  tile_size = tile_size_;
}

Tileset::~Tileset()
{
  std::cout << "Tileset: destroy" << std::endl;
}

#ifdef SWIGGUILE
void
Tileset::load_tile_file(const std::string& filename)
{
  SCM input_stream = scm_open_file(gh_str02scm(filename.c_str()), 
                                   gh_str02scm("r"));
  SCM tree = scm_read(input_stream);
  
  if (!(gh_symbol_p(gh_car(tree)) && gh_equal_p(gh_symbol2scm("windstille-tiles"), gh_car(tree))))
    {
      std::cout << "Not a Windstille Tile File!" << std::endl;
    }
  else
    {
      tree = gh_cdr(tree);

      while (!gh_null_p(tree))
        {
          SCM current = gh_car(tree);
          
          if (gh_pair_p(current))
            {
              SCM name    = gh_car(current);
              SCM data    = gh_cdr(current);
      
              if (gh_equal_p(gh_symbol2scm("tile"), name)) 
                {
                  add_tile(data);
                }
              else
                {
                  std::cout << "Tileset: Unknown tag: " << scm2string(name) << std::endl;
                }
            }
          else
            {
              std::cout << "Tileset: Not a pair!"  << std::endl;
            }
          tree = gh_cdr(tree);
        }
    }
}

void
Tileset::add_tile(SCM data)
{
  // FIXME: Move this to scripting and add a Tileset::add()
  int id = 0;
  std::string image;
  CL_Color color(254, 254, 254, 254);
  CL_Color attribute_color(255, 255, 255, 100);
  unsigned char colmap[8];
  
  while (!gh_null_p(data))
    {
      SCM current = gh_car(data);
          
      if (gh_pair_p(current))
        {
          SCM name    = gh_car(current);
          SCM data    = gh_cdr(current);

          if (gh_equal_p(gh_symbol2scm("id"), name))           
            {
              id = gh_scm2int(gh_car(data));
            }
          else if (gh_equal_p(gh_symbol2scm("color"), name))
            {
              color = CL_Color(gh_scm2int(gh_car(data)),
                               gh_scm2int(gh_cadr(data)),
                               gh_scm2int(gh_caddr(data)),
                               gh_scm2int(gh_car(gh_cdddr(data))));
            }
          else if (gh_equal_p(gh_symbol2scm("attribute-color"), name))
            {
              attribute_color = CL_Color(gh_scm2int(gh_car(data)),
                                         gh_scm2int(gh_cadr(data)),
                                         gh_scm2int(gh_caddr(data)),
                                         gh_scm2int(gh_car(gh_cdddr(data))));
            }
          else if (gh_equal_p(gh_symbol2scm("image"), name))
            {
              image = scm2string(gh_car(data));
            }
          else if (gh_equal_p(gh_symbol2scm("colmap"), name))
            {
              colmap[0] = gh_scm2int(gh_car(data));
              data = gh_cdr(data);
              colmap[1] = gh_scm2int(gh_car(data));
              data = gh_cdr(data);
              colmap[2] = gh_scm2int(gh_car(data));
              data = gh_cdr(data);
              colmap[3] = gh_scm2int(gh_car(data));
              data = gh_cdr(data);
              colmap[4] = gh_scm2int(gh_car(data));
              data = gh_cdr(data);
              colmap[5] = gh_scm2int(gh_car(data));
              data = gh_cdr(data);
              colmap[6] = gh_scm2int(gh_car(data));
              data = gh_cdr(data);
              colmap[7] = gh_scm2int(gh_car(data));
            }
        }
      data = gh_cdr(data);
    }

  if (0) // Debugging code
    {
      std::cout << "Tile: id     = " << id << "\n"
                << "      image  = " << image << "\n"
                << "      colmap = " 
                << int(colmap[0]) << ", "
                << int(colmap[1]) << ", "
                << int(colmap[2]) << ", "
                << int(colmap[3]) << ", "
                << int(colmap[4]) << ", "
                << int(colmap[5]) << ", "
                << int(colmap[6]) << ", "
                << int(colmap[7])
                << std::endl;
    }

  if (id < 0)
    std::cout << "Tile Id bug: " << id << std::endl;
  else
    {
      if (id >= int(tiles.size()))
        {
          tiles.resize(id+1);
        }
      tiles[id] = new Tile(image, color, attribute_color, colmap);
      tiles[id]->id = id;
    }
}
#endif

Tile* 
Tileset::create (int id)
{
  // FIXME: this might cause throuble for some games
  // id 0 is always the empty tile
  if (id == 0)
    { 
      return 0;
    }
  else
    {
      if (id > 0 && id < int(tiles.size()))
        return tiles[id];
      else
        return 0;
    }
}

void
Tileset::init()
{
  assert(current_ == 0);
}

/** Destroy the default TileFactor*/
void
Tileset::deinit()
{
  delete current_;
}

/* EOF */
