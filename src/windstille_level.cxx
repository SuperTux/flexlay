//  $Id: windstille_level.cxx,v 1.1 2003/08/10 19:56:40 grumbel Exp $
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
#include "xmlhelper.hxx"

template<class T>
T xmlGetValue (xmlNodePtr cur, const std::string& str) 
{
  char* value = (char*)xmlGetProp(cur, (xmlChar*)str.c_str ());
  if (value) {
    T int_value;
    from_string (value, int_value);
    free (value);
    return int_value;
  } else {
    std::cout << "Error!" << std::endl;
    return T ();
  }
}

WindstilleLevel::WindstilleLevel (const std::string& filename)
  : field (0)
{
  parse_file (filename);
}

void 
WindstilleLevel::parse_file (const std::string& filename)
{
  std::cout << "Windstille Level: " << filename << std::endl;
  doc = xmlParseFile(filename.c_str());
  
  if (doc == NULL)
    {
      assert (!"Error: Levelfile not found");
    }

  xmlNodePtr cur = xmlDocGetRootElement(doc);
  
  //if (xmlIsBlankNode(cur)) cur = cur->next;

  if (cur != NULL && strcmp((const char*)cur->name, "turrican-level") == 0)
    {
      if (xmlIsBlankNode(cur)) cur = cur->next;
      
      if (cur->children == NULL)
	std::cout << "XMLPLF: node: " << cur->name << std::endl;
      
      cur = cur->children;
      while (cur != NULL)
	{
	  if (xmlIsBlankNode(cur)) {
	    cur = cur->next;
	    continue;
	  } else if (strcmp((char*)cur->name, "properties") == 0) {
	    parse_properties (cur);
	  } else if (strcmp((char*)cur->name, "tilemap") == 0) {
	    parse_tilemap (cur);
	  } else if (strcmp((char*)cur->name, "gameobjects") == 0) {
	    parse_gameobjects (cur);
	  } else {
	    std::cout << "Error: Unknown tag: " << cur->name << std::endl;
	  }
	  cur = cur->next;
	}
    }
  else
    {
      std::cout << "Not a Windstille Level" << std::endl;
      exit(EXIT_FAILURE);
    }
}

void
WindstilleLevel::parse_properties (xmlNodePtr cur)
{
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
}


void
WindstilleLevel::parse_tilemap (xmlNodePtr cur)
{
  int width  = xmlGetValue<int> (cur, "width");
  int height = xmlGetValue<int> (cur, "height");

  int x = 0;
  int y = 0;

  std::cout << "TileMap: " << width << "x" << height << std::endl;
  
  field = new Field<std::string>(width, height);

  cur = cur->children;

  while (cur != NULL)
    {
      if (xmlIsBlankNode(cur)) {
	cur = cur->next;
	continue;
      } else if (strcmp((char*)cur->name, "row") == 0) {
	xmlNodePtr ccur = cur->children;
	while (ccur != NULL)
	  {
	    if (xmlIsBlankNode(ccur)) {
	      ccur = ccur->next;
	      continue;
	    } else if (strcmp((char*)ccur->name, "tile") == 0) {
	      std::string str (XMLhelper::parse_string (doc, ccur));
	      //std::cout << "str: " << str << std::endl;
	      assert(x < width);
	      assert(y < height);
	      (*field)(x, y) = str;
	      ++x;
	    } else {
	      std::cout << "Error: parse_properties:row Unknown tag: " << ccur->name << std::endl;	      
	    }
	    ccur = ccur->next;
	  }
	++y;
	x = 0;
      } else {
	std::cout << "Error: parse_properties: Unknown tag: " << cur->name << std::endl;
      }
      cur = cur->next;
    }

  if (x != 0 || y != height)
    {
      std::cout << "Error: Tilemap incomplete" << std::endl;
    }
}

void
WindstilleLevel::parse_gameobjects (xmlNodePtr cur)
{
  cur = cur->children;
  while (cur != NULL)
    {
      if (xmlIsBlankNode(cur)) {
	cur = cur->next;
	continue;
      } else if (strcmp((char*)cur->name, "guile-object") == 0) {
	std::string name = xmlGetValue<std::string> (cur, "name");
	std::cout << "Parsing of guile-gameobjects not implemented" << std::endl;

      } else {
	std::cout << "Error: parse_gameobjects: Unknown tag: " << cur->name << std::endl;
      }
      cur = cur->next;
    }
}

/* EOF */
