//  -*- mode: clanlib -*-
//  $Id: args_parse.cxx,v 1.6 2003/09/07 21:01:45 grumbel Exp $
//
//  Pingus - A free Lemmings clone
//  Copyright (C) 2002 Ingo Ruhnke <grumbel@gmx.de>
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

#include <iomanip>
#include <iostream>
#include <sstream>
#include <ClanLib/Core/System/error.h>
#include <ClanLib/Core/System/clanstring.h>
#include "args_parse.hxx"
#include "args_parse_generic.hxx"

CL_ArgsParse::CL_ArgsParse()
{
	impl = new CL_ArgsParse_Generic();
}

CL_ArgsParse::~CL_ArgsParse()
{
	delete impl;
}

void
CL_ArgsParse::parse_args(int argc, char** argv)
{
	impl->parse_args(argc, argv);
}

void
CL_ArgsParse::print_help()
{
	impl->print_help();
}

void
CL_ArgsParse::set_help_indent(int i)
{
	impl->set_help_indent(i);
}

void
CL_ArgsParse::add_usage(const std::string& usage)
{
	impl->add_usage(usage);
}

void
CL_ArgsParse::add_doc(const std::string& doc)
{
	impl->add_doc(doc);
}

void
CL_ArgsParse::add_group(const std::string& grouptopic)
{
	impl->add_group(grouptopic);
}

void
CL_ArgsParse::add_option(int key, 
                      const std::string& long_option, 
                      const std::string& argument,
                      const std::string& help,
                      bool visible)
{
	impl->add_option(key, long_option, argument, help, visible);
}

bool
CL_ArgsParse::next()
{
	impl->next();
}

int
CL_ArgsParse::get_key()
{
	return impl->get_key();
}

std::string
CL_ArgsParse::get_argument()
{
	return impl->get_argument();
}

/* EOF */
