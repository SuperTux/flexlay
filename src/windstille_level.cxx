//  $Id: windstille_level.cxx,v 1.2 2003/08/10 22:55:50 grumbel Exp $
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

#include <config.h>
#include <iostream>
#include "windstille_level.hxx"
#include "string_converter.hxx"
#include "scm_helper.hxx"

WindstilleLevel::WindstilleLevel (const std::string& filename)
  : field (0)
{
  parse_file (filename);
}

void 
WindstilleLevel::parse_file (const std::string& filename)
{
  std::cout << "Windstille Level: " << filename << std::endl;
  
  SCM input_stream = scm_open_file(gh_str02scm(filename.c_str()), 
                                   gh_str02scm("r"));
  SCM tree = scm_read(input_stream);
  
  if (!(gh_symbol_p(gh_car(tree)) && gh_equal_p(gh_symbol2scm("windstille-level"), gh_car(tree))))
    {
      std::cout << "Not a Windstille Level file!" << std::endl;
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
                  parse_tilemap(data);
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

}

void
WindstilleLevel::parse_properties (SCM cur)
{
#if 0
  std::cout << "Parsinc properties" << std::endl;

  cur = cur->children;

  while (cur != NULL)
    {
      if (xmlIsBlankNode(cur)) {
	cur = cur->next;
	continue;
      } else {
	std::cout << "Error: parse_properties: Unknown tag: " << cur->name << std::endl;
      }
      cur = cur->next;
    }
#endif
}


void
WindstilleLevel::parse_tilemap (SCM cur)
{
  gh_display(cur);
  int width  = gh_scm2int(gh_cadar(cur));
  int height = gh_scm2int(gh_car(gh_cdadr(cur)));
  
  std::cout << "Size: " << width << "x" << height << std::endl;
  
  field = new Field<std::string>(width, height);

  cur = gh_cddr(cur);
  
  int x = 0;
  int y = 0;
  while (!gh_null_p(cur))
    {
      SCM name = gh_caar(cur);
      SCM data = gh_cdar(cur);
      
      if (gh_equal_p(gh_symbol2scm("row"), name))
        {
          x = 0;
          while (!gh_null_p(data))
            {
              std::string str = scm2string(gh_cadar(data));
              std::cout << "Tile: " << str << std::endl;
              (*field)(x, y) = str;
              data = gh_cdr(data);
              x += 1;
            }
          y += 1;
        }
          
      cur = gh_cdr(cur);
    }
}

void
WindstilleLevel::parse_gameobjects (SCM cur)
{
}

/* EOF */
