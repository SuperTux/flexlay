//  $Id: XMLhelper.hxx,v 1.1 2002/03/19 17:56:54 grumbel Exp $
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

#ifndef XMLHELPER_HH
#define XMLHELPER_HH

#include <config.h>
#include <string>
#include <fstream>
#include <ClanLib/core.h>

#include <libxml/parser.h>

// Some compatibility stuff
#ifdef LIBXML_2
#define ROOT children
#else // libxml 1.x
#define children childs
#define ROOT root
#endif

// Hack: if xmlIsBlankNode() is not present, we define an empty dummy
#ifdef NO_XMLISBLANKNODE
int xmlIsBlankNode(xmlNodePtr node);
#endif

class XMLhelper
{
private:

public:
  static std::string XMLhelper::encode_entities(const std::string& arg_str);

  /// A set of function to parse an xml file
  //@{
  static CL_Vector     parse_vector(xmlDocPtr doc, xmlNodePtr cur);
  static std::string   parse_string(xmlDocPtr doc, xmlNodePtr cur);
  static int           parse_int(xmlDocPtr doc, xmlNodePtr cur);
  static float         parse_float(xmlDocPtr doc, xmlNodePtr cur);
  //@}

  /// A set of functions to write data down to an xml file
  //@{
  /** Write a CL_Vector to an xml stream */
  static void write_vector_xml(std::ofstream* xml, const CL_Vector& pos);
  //@}
};

#endif

/* EOF */
