//  $Id: XMLhelper.cxx,v 1.1 2002/03/19 17:56:54 grumbel Exp $
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

#include "StringConverter.hxx"
#include "XMLhelper.hxx"

// Hack: if xmlIsBlankNode() is not present, we define an empty dummy
#ifdef NO_XMLISBLANKNODE
int xmlIsBlankNode(xmlNodePtr node) { return 0; }
#endif

std::string
XMLhelper::encode_entities(const std::string& arg_str)
{
  ///xmlEncodeEntitiesReentrant()
  std::string str = arg_str;
  std::string::size_type i;
  //  std::cout << "encode_xml: " << str << std::endl;
 
  i = str.find("<");
  if (i != std::string::npos)
    str.replace(i, 1, "&lt;");

  i = str.find(">");
  if (i != std::string::npos)
    str.replace(i, 1, "&gt;");

  //  std::cout << "encode_xml-done: " << str << std::endl;

  return str;
}




CL_Vector
XMLhelper::parse_vector(xmlDocPtr doc, xmlNodePtr cur)
{
  CL_Vector pos;
  cur = cur->children;  
  while (cur != NULL)
    {
      if (xmlIsBlankNode(cur)) 
	{
	  cur = cur->next;
	  continue;
	}

      char* ident = (char*)xmlNodeListGetString(doc, cur->children, 1);

      if (ident)
	{
	  //std::cout << "parse_position: ident = " << ident << std::endl;
	  if (strcmp((char*)cur->name, "x-pos") == 0) {
	    from_string (ident, pos.x);
	  } else if (strcmp((char*)cur->name, "y-pos") == 0) {
	    from_string (ident, pos.y);
	  } else if (strcmp((char*)cur->name, "z-pos") == 0) {
	    from_string (ident, pos.z);
	  } else {
	    std::cout << "Unhandled position ident: " << ident << std::endl;
	  }
	  free(ident);
	}
      cur = cur->next;
    }
  return pos;
}

int
XMLhelper::parse_int(xmlDocPtr doc, xmlNodePtr cur)
{
  cur = cur->children;
  
  int number = 999;
  char* number_str = (char*)xmlNodeListGetString(doc, cur, 1);
  if (number_str) {
    from_string(number_str, number);
    free(number_str);
  } else {
    std::cout << "Error: XMLhelper: parse_int: Field empty" << std::endl;
  }
  return number;
}

float
XMLhelper::parse_float(xmlDocPtr doc, xmlNodePtr cur)
{
  cur = cur->children;
  
  float number = 3.1415927f;
  char* number_str = (char*)xmlNodeListGetString(doc, cur, 1);
  if (number_str) {
    from_string(number_str, number);
    free(number_str);
  } else {
    std::cout << "XMLhelper: parse_int: Field empty" << std::endl;
  }
  return number;
}

std::string 
XMLhelper::parse_string(xmlDocPtr doc, xmlNodePtr cur)
{
  std::string ret_str;
  cur = cur->children;

  char* str = (char*)xmlNodeListGetString(doc, cur, 1);
  if (str) 
    {
      ret_str = str;
      free(str);
      return ret_str;
    }
  else
    {  
      std::cout << "XMLhelper::parse_string: Field empty" << std::endl;
      return "";
    }
}



void 
XMLhelper::write_vector_xml(std::ofstream* xml, const CL_Vector& pos)
{
  (*xml) << "  <position>\n"
	 << "    <x-pos>" << pos.x << "</x-pos>\n"
	 << "    <y-pos>" << pos.y << "</y-pos>\n"
	 << "    <z-pos>" << pos.z << "</z-pos>\n"
	 << "  </position>\n";
}

/* EOF */
