//  -*- mode: clanlib -*-
//  $Id: args_parse_generic.hxx,v 1.1 2003/09/06 20:38:18 grumbel Exp $
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

#ifndef HEADER_WINDSTILLE_ARGS_PARSE_GENERIC_HXX
#define HEADER_WINDSTILLE_ARGS_PARSE_GENERIC_HXX

#include <string>
#include <vector>
#include "args_parse.hxx"

class CL_ArgsParse_Generic
{
private:
	int help_indent;

	std::string programm;

	struct Option 
	{
		int key;
		std::string help;
		std::string long_option;
		std::string argument;
		bool visible;
	};
  
	typedef std::vector<Option> Options;
	Options options;

public:
private:
	typedef std::vector<CL_ArgsParse::Option> ParsedOptions;
	ParsedOptions parsed_options;

	static const int GROUP     = -3;
	static const int DOC       = -4;
	static const int USAGE     = -5;

public:  
	CL_ArgsParse::iterator begin() { return parsed_options.begin(); }
	CL_ArgsParse::iterator end()   { return parsed_options.end(); }

	static const int REST_ARG  = -2;

	CL_ArgsParse_Generic();

	void set_help_indent(int i) { help_indent = i; }

	void add_usage(const std::string& usage);
	void add_doc(const std::string& doc);
	void add_group(const std::string& grouptopic);
  
	void add_option(int key,
						 const std::string& long_option, 
						 const std::string& argument,
						 const std::string& help,
						 bool visible = true);

	void parse_args(int argc, char** argv);
	void print_help();
  
private:
	void read_option(int id, const std::string& argument);

	/** Find the Option structure that matches \a short_option */
	Option* lookup_short_option(char short_option);

	/** Find the Option structure that matches \a long_option */
	Option* lookup_long_option (const std::string& long_option);

	CL_ArgsParse_Generic (const CL_ArgsParse_Generic&);
	CL_ArgsParse_Generic& operator= (const CL_ArgsParse_Generic&);
};

#endif

/* EOF */
