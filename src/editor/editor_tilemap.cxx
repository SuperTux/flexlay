//  $Id: editor_tilemap.cxx,v 1.1 2003/08/10 19:56:40 grumbel Exp $
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

#include <iostream>
#include "../windstille_level.hxx"
#include "../globals.hxx"
#include "editor_tilemap.hxx"

EditorTileMap::EditorTileMap (int width, int height)
  : field (new Field<EditorTile*> (width, height))
{
  for (unsigned int y = 0; y < field->get_height (); ++y) {
    for (unsigned int x = 0; x < field->get_width (); ++x)
      {
	field->at (x, y) = new EditorTile ("none");
      }
  }  
}

EditorTileMap::EditorTileMap (const std::string& filename)
{
  WindstilleLevel data (filename);

  field = new Field<EditorTile*> (data.get_field ()->get_width (),
				  data.get_field ()->get_height ());

  for (unsigned int y = 0; y < field->get_height (); ++y) {
    for (unsigned int x = 0; x < field->get_width (); ++x)
      {
	std::string name = data.get_field()->at(x, y);
	field->at (x, y) = new EditorTile (name);
      }
  }
}
  
void
EditorTileMap::draw ()
{
  for (unsigned int y = 0; y < field->get_height (); ++y)
    {
      for (unsigned int x = 0; x < field->get_width (); ++x)
	{
	  field->at (x, y)->draw (x * 64, y * 64);
	}
    }
}

EditorTile*
EditorTileMap::get_tile (int x, int y)
{
  if (x >= 0 && x < (int) field->get_width () &&
      y >= 0 && y < (int) field->get_height ())
    return field->at (x, y);
  else
    return 0;
}

void
EditorTileMap::save (const std::string& filename)
{
#if 0
  std::ofstream out (filename.c_str ());
  
  if (out)
    {
      out << "<windstille-level>\n"
	  << "  <properties>\n"
	  << "    <levelname>Bla</levelname>\n"
	  << "  </properties>\n" << std::endl;

      out << "<tilemap width=\"" << field->get_width () << "\" height=\"" << field->get_height () << "\">";

      for (unsigned int y = 0; y < field->get_height (); ++y)
	{
	  out << "<row>" << std::endl;
	  for (unsigned int x = 0; x < field->get_width (); ++x)
	    {
	      out << "<tile>" << field->at (x, y)->get_name () << "</tile>" << std::endl;
	    }
	  out << "</row>" << std::endl;
	}

      out << "</tilemap>";
      out << "</windstille-level>" << std::endl;
    }
  else
    {
      std::cout << "Write error" << std::endl;
    }
#endif
}

/* EOF */
