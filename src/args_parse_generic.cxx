//  -*- mode: clanlib -*-
//  $Id: args_parse_generic.cxx,v 1.2 2003/09/07 21:01:45 grumbel Exp $
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
#include "args_parse_generic.hxx"

CL_ArgsParse_Generic::CL_ArgsParse_Generic()
{
	help_indent = 18;
}

void
CL_ArgsParse_Generic::parse_args(int argc, char** argv)
{
	programm = argv[0];

	for(int i = 1; i < argc; ++i) 
	{
		if (argv[i][0] == '-') 
		{
			if (argv[i][1] == '-') 
			{
				// We got a long option
				if (argv[i][2] == '\0') { 
					// Got a '--', so we stop evaluating arguments
					++i;
					while(i < argc) 
					{
						read_option(CL_ArgsParse::REST_ARG, argv[i]);
						++i;
					}
				} 
				else
				{
					std::string opt = argv[i] + 2;
					std::string long_opt;
					std::string long_opt_arg;

					std::string::size_type pos = opt.find('=');

					if (pos != std::string::npos) 
					{
						long_opt = opt.substr(0, pos);
						long_opt_arg = opt.substr(pos+1);
					}
					else 
					{
						long_opt = opt;
					}

					// Long Option
					Option* option = lookup_long_option(long_opt);

					if (option) 
					{
						if (option->argument.empty()) 
						{
							read_option(option->key, "");
						} 
						else
						{
							if (pos != std::string::npos) 
							{
								read_option(option->key, long_opt_arg);
							}
							else
							{            
								if (i == argc - 1) 
								{
									throw CL_Error("option '" + std::string(argv[i]) + "' requires an argument");
								}
								else 
								{
									read_option(option->key, argv[i + 1]);
									++i;
								}
							}
						}
					}
					else
					{
						throw CL_Error("unrecognized option '" + std::string(argv[i]) + "'");
					}
				}
			} 
			else 
			{
				// We got a short option
				char* p = argv[i] + 1;
          
				if (*p != '\0') {
					// Handle option chains
					while (*p) 
					{
						// Short option(s)
						Option* option = lookup_short_option(*p);

						if (option) 
						{
							if (option->argument.empty()) 
							{
								read_option(option->key, "");
							} 
							else 
							{
								if (i == argc - 1 || *(p+1) != '\0') 
								{
									// No more arguments
									throw CL_Error("option requires an argument -- " + std::string(1, *p));
								}
								else
								{
									read_option(option->key, argv[i + 1]);
									++i;
								}
							}
						} 
						else 
						{
							throw CL_Error("invalid option -- " + std::string(1, *p));
						}
						++p; 
					}
				} 
				else
				{
					read_option(CL_ArgsParse::REST_ARG, "-");
				} 
			}
		} 
		else
		{
			read_option(CL_ArgsParse::REST_ARG, argv[i]);
		}
	}

	current_option = parsed_options.end();
}

CL_ArgsParse_Generic::Option*
CL_ArgsParse_Generic::lookup_short_option(char short_option)
{
	for(Options::iterator i = options.begin(); i != options.end(); ++i)
	{
		if (i->key == short_option)
			return &(*i);
	}
	return 0;
}

CL_ArgsParse_Generic::Option*
CL_ArgsParse_Generic::lookup_long_option(const std::string& long_option)
{
	for(Options::iterator i = options.begin(); i != options.end(); ++i)
	{
		if (i->long_option == long_option)
			return &*i;
	}
	return 0;
}

void
CL_ArgsParse_Generic::read_option(int key, const std::string& argument)
{
	ParsedOption parsed_option;
  
	parsed_option.key = key;
	parsed_option.argument = argument;

	parsed_options.push_back(parsed_option);
}

void
CL_ArgsParse_Generic::print_help()
{
	std::ostringstream str;

	bool first_usage = true;
	for(Options::iterator i = options.begin(); i != options.end(); ++i)
	{
		if (i->visible)
		{
			if (i->key == USAGE) 
			{
				if (first_usage) 
				{
					std::cout << "Usage: " << programm << " " <<  i->help << std::endl; 
					first_usage = false;
				}
				else
				{
					std::cout << "or:    " << programm << " " << i->help << std::endl; 
				}
			} 
			else if (i->key == GROUP) 
			{
				if (i != options.begin())
					std::cout << std::endl;
				std::cout << i->help << std::endl;
			}
			else if (i->key == DOC) 
			{
				if (i != options.begin())
					std::cout << std::endl;
				std::cout << i->help << std::endl;
			}
			else 
			{
				str.str("");
				if (i->key > 255 || i->key < 0)
					str << "--" <<  i->long_option;
				else if (i->long_option.empty())
					str << "-" << char(i->key);
				else
					str << "-" << char(i->key) << ", --" << i->long_option;

				if (!i->argument.empty())
				{
					if (i->long_option.empty())
						str << " " << i->argument;
					else
						str << "=" << i->argument;
				}

				std::cout << "  " 
							 << std::setiosflags(std::ios::left) << std::setw(help_indent) << str.str() << std::setw(0)
							 << " " << i->help << std::endl;
			}
		}
	}
	std::cout << std::endl;
}

void
CL_ArgsParse_Generic::add_usage(const std::string& usage)
{
	Option option;

	option.key          = USAGE;
	option.help         = usage;
	option.visible      = true;

	options.push_back(option);   
}

void
CL_ArgsParse_Generic::add_doc(const std::string& grouptopic)
{
	Option option;

	option.key          = DOC;
	option.help         = grouptopic;
	option.visible      = true;

	options.push_back(option);  
}

void
CL_ArgsParse_Generic::add_group(const std::string& grouptopic)
{
	Option option;

	option.key          = GROUP;
	option.help         = grouptopic;
	option.visible      = true;

	options.push_back(option);  
}

void
CL_ArgsParse_Generic::add_option(int key, 
                      const std::string& long_option, 
                      const std::string& argument,
                      const std::string& help,
                      bool visible)
{
	Option option;

	option.key          = key;
	option.help         = help;
	option.long_option  = long_option;
	option.argument     = argument;
	option.visible      = visible;

	options.push_back(option);
}

bool
CL_ArgsParse_Generic::next()
{
	if (current_option == parsed_options.end()) 
	{
		(current_option = parsed_options.begin());
		return current_option != parsed_options.end();
	}
	else
	{
		return (++current_option) != parsed_options.end();
	}
}

int
CL_ArgsParse_Generic::get_key()
{
	return current_option->key;
}

std::string
CL_ArgsParse_Generic::get_argument()
{
	return current_option->argument;
}

/* EOF */
