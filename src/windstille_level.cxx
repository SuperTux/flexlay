//  $Id: windstille_level.cxx,v 1.13 2003/11/04 22:48:51 grumbel Exp $
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

#include <config.h>
#include <assert.h>
#include <iostream>
#include "globals.hxx"
#include "windstille_level.hxx"
#include "string_converter.hxx"
#include "scm_helper.hxx"

WindstilleLevel::WindstilleLevel (const std::string& filename)
  : tilemap(0),
    background_tilemap(0)
{
  width  = 50;
  height = 50;
  parse_file (filename);
}

void 
WindstilleLevel::parse_file (const std::string& filename)
{
  diamond_map = 0;

  if (debug)
    std::cout << "Windstille Level: " << filename << std::endl;
  
  SCM input_stream = scm_open_file(gh_str02scm(filename.c_str()), 
                                   gh_str02scm("r"));
  SCM tree = scm_read(input_stream);
  
  if (!(gh_symbol_p(gh_car(tree)) && gh_equal_p(gh_symbol2scm("windstille-level"), gh_car(tree))))
    {
      std::cout << filename << ": not a Windstille Level file!" << std::endl;
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
      
              if (gh_equal_p(gh_symbol2scm("tilemap"), name)) 
                {
                  parse_foreground_tilemap(data);
                }
              else if (gh_equal_p(gh_symbol2scm("background-tilemap"), name)) 
                {
                  parse_background_tilemap(data);
                }
              else if (gh_equal_p(gh_symbol2scm("water"), name)) 
                {
                  parse_water(data);
                }
              else if (gh_equal_p(gh_symbol2scm("properties"), name))
                {
                  parse_properties(data);
                }
              else if (gh_equal_p(gh_symbol2scm("diamond-map"), name)) 
                {
                  parse_diamond_map(data);
                }
              else if (gh_equal_p(gh_symbol2scm("scripts"), name)) 
                {
                  parse_scripts(data);
                }
              else
                {
                  std::cout << "WindstilleLevel: Unknown tag: " << scm2string(name) << std::endl;
                }
            }
          else
            {
              std::cout << "WindstilleLevel: Not a pair!"  << std::endl;
            }
          tree = gh_cdr(tree);
        }
    }
  if (!diamond_map)
    {
      std::cout << "No diamond map in level file" << std::endl;
      diamond_map = new Field<int>(width * 2, height * 2);
    }
}

void
WindstilleLevel::parse_water(SCM tree)
{
  while (!gh_null_p(tree))
    {
      SCM current = gh_car(tree);

      if (gh_pair_p(current))
        {
          SCM name    = gh_car(current);
          SCM data    = gh_cdr(current);
      
          if (gh_equal_p(gh_symbol2scm("water"), name)) 
            {
              //gh_display(data);
              //gh_newline();
              int x = gh_scm2int(gh_car(data));
              int y = gh_scm2int(gh_cadr(data));
              int w = gh_scm2int(gh_caddr(data));
              int h = gh_scm2int(gh_car(gh_cdddr(data)));
              std::cout << "Water: " << x << " " << y << " " << w << " " << h << std::endl;
            }

          tree = gh_cdr(tree);
        }
    }  
}

void
WindstilleLevel::parse_properties (SCM tree)
{
  while (!gh_null_p(tree))
    {
      SCM current = gh_car(tree);

      if (gh_pair_p(current))
        {
          SCM name    = gh_car(current);
          SCM data    = gh_cadr(current);
      
          if (gh_equal_p(gh_symbol2scm("width"), name)) 
            {
              width  = gh_scm2int(data);
            }
          else if (gh_equal_p(gh_symbol2scm("height"), name))
            {
              height = gh_scm2int(data);
            }
          else
            {
              std::cout << "WindstilleLevel::parse_properties: Unknown tag: " 
                        << std::endl;
            }

          tree = gh_cdr(tree);
        }
    }

  if (debug)
    std::cout << "WindstilleLevel: dimensions: " << width << "x" << height << std::endl;
}

void
WindstilleLevel::parse_background_tilemap (SCM cur)
{
  background_tilemap = parse_tilemap(cur);
}

void
WindstilleLevel::parse_foreground_tilemap (SCM cur)
{
  tilemap = parse_tilemap(cur);
}

Field<int>* 
WindstilleLevel::parse_tilemap (SCM cur)
{
  Field<int>* field = new Field<int>(width, height);
  
  int x = 0;
  int y = 0;
  while (!gh_null_p(cur) && y < height)
    {
      SCM name = gh_caar(cur);
      SCM data = gh_cdar(cur);
      
      if (gh_equal_p(gh_symbol2scm("data"), name))
        {
          while (!gh_null_p(data) && y < height)
            {
              int id = gh_scm2int(gh_car(data));
              (*field)(x, y) = id;
              
              x += 1;

              if (x >= width)
                {
                  x = 0;
                  y += 1;
                }
              
              data = gh_cdr(data);
            }
          if (y != height)
            std::cout << "WindstilleLevel: Something went wrong: y=" << y << " height=" << height << std::endl;
        }
          
      cur = gh_cdr(cur);
    }
  return field;
}

void
WindstilleLevel::parse_diamond_map(SCM data)
{
  diamond_map = new Field<int>(width * 2, height * 2);

  for(Field<int>::iterator i = diamond_map->begin(); i != diamond_map->end(); ++i)
    {
      *i = false;
    }
  
  int x = 0;
  int y = 0;

  while (!gh_null_p(data) && y < height*2)
    {
      (*diamond_map)(x, y) = gh_scm2int(gh_car(data));
              
      x += 1;

      if (x >= width*2)
        {
          x = 0;
          y += 1;
        }
              
      data = gh_cdr(data);
    }

  if (y != height*2)
    std::cout << "WindstilleLevel: Something went wrong: y=" << y << " height=" << height << std::endl;
}

void
WindstilleLevel::parse_scripts(SCM data)
{
  while (!gh_null_p(data))
    {
      char* str = gh_scm2newstr(gh_car(data), 0);
      scripts.push_back(str);
      free(str);

      data = gh_cdr(data);
    }
}

void
WindstilleLevel::parse_gameobjects (SCM cur)
{
}

/* EOF */
