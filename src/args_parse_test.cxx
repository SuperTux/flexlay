//  -*- mode: clanlib -*-
//  $Id: args_parse_test.cxx,v 1.6 2003/09/07 21:01:45 grumbel Exp $
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

#include <iostream>
#include <ClanLib/Core/System/error.h>
#include "args_parse.hxx"

class MyArgsParse
{
public:
	static const int ARG_ARG = 301;

	CL_ArgsParse argp;

	MyArgsParse(int argc, char** argv) 
	{
		argp.add_usage("[OPTIONS]... [FILES]...");
		argp.add_usage("foobar [OPTIONS]... [FILES]...");
	 
		argp.add_doc("This programm does nothing usefull..");
	 
		argp.add_group ("Options that want a file:");
		argp.add_option('c',	  "config", "FILE", "Config the app");
		argp.add_option('f',	  "file",	"FILE", "Load a file");

		argp.add_group ("Options that don't want a file:");
		argp.add_option('z',	  "zero",	"",	  "Zero Args");
		argp.add_option('a',	  "",		 "",	  "short a");
		argp.add_option('h',	  "help",	"",	  "help");

		argp.add_group ("Options without a short arg:");
		argp.add_option(ARG_ARG, "arg",	 "",	  "long a");
	 
		argp.add_doc("For more info have a look at FOobar does nothing usefull..\n"
						 "Examples can be found at blubbark...\n"
						 "....");

		argp.parse_args(argc, argv);
	}
  
	void read_options() 
	{
		while (argp.next())
		{
			switch (argp.get_key()) 
			{
			case 'h':
				argp.print_help();
				break;
			case 'f':
				std::cout << "file: " << argp.get_argument() << std::endl;
				break;
			case 'c':
				std::cout << "config: " << argp.get_argument() << std::endl;
				break;
			case 'a':
				std::cout << "a" << std::endl;
				break;
			case 'z':
				std::cout << "zero" << std::endl;
				break;
			case ARG_ARG:
				std::cout << "arg" << std::endl;
				break;
			case CL_ArgsParse::REST_ARG:
				std::cout << "rest: " << argp.get_argument() << std::endl;
				break;
			default:
				std::cout << "Got " << argp.get_key() << " " << argp.get_argument() << std::endl;
				break;
			}
		}
	}
};

int main(int argc, char** argv)
{
	try 
	{
		MyArgsParse args(argc, argv);
		args.read_options();
	} 
	catch (CL_Error& err) 
	{
		std::cout << argv[0] << err.message << std::endl;
	}
}

/* EOF */
