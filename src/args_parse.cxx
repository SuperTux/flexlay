//  $Id: args_parse.cxx,v 1.2 2003/09/06 11:14:16 grumbel Exp $
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
#include <ClanLib/Core/System/clanstring.h>
#include "args_parse.hxx"

ArgsParse::ArgsParse()
{
}

void
ArgsParse::parse_args(int arg_argc, char** arg_argv)
{
  argc = arg_argc;
  argv = arg_argv;

  programm = argv[0];

  int i = 1;
  while (i < argc)
    {
      i = parse_arg(i);
    }
}

std::pair<std::string, std::string>
ArgsParse::split_long_option(const std::string& option)
{
  std::string::size_type i = option.find('=');
  if (i != std::string::npos) {
    return std::pair<std::string, std::string>(option.substr(0, i),
                                               option.substr(i+1));
  } else {
    return std::pair<std::string, std::string>(option, "");
  }
}

int
ArgsParse::parse_arg(int i)
{
  if (argv[i][0] == '-') {
    if (argv[i][1] == '-') {
      if (argv[i][2] == '\0') { 
        // Got a '--', so we stop evaluating arguments
        ++i;
        while(i < argc) {
          read_option(REST_ARG, argv[i]);
          ++i;
        }
        return i;
      } else {
        std::string opt = argv[i] + 2;
        std::string long_opt;
        std::string long_opt_arg;

        std::string::size_type pos = opt.find('=');

        if (pos != std::string::npos) {
          long_opt = opt.substr(0, pos);
          long_opt_arg = opt.substr(pos+1);
        } else {
          long_opt = opt;
        }

        // Long Option
        Option* option = lookup_long_option(long_opt);

        if (option) {
          if (option->argument.empty()) {
            read_option(option->key, "");
          } else {
            if (pos != std::string::npos) {
              read_option(option->key, long_opt_arg);
            } else {            
              if (i == argc - 1) {
                parse_error(std::string(argv[i]) + " requires argument: " + option->argument);
              } else {
                read_option(option->key, argv[i + 1]);
                return i+2;
              }
            }
          }
        } else {
          parse_error("Unknown long option '" + std::string(argv[i]) + "'");
        }
      }
    } else {
      char* p = argv[i] + 1;
          
      if (p) {
        // Handle option chains
        while (*p) {
          // Short option(s)
          Option* option = lookup_short_option(*p);

          if (option) {
            if (option->argument.empty()) {
              read_option(option->key, "");
            } else {
              if (i == argc - 1 || *(p+1) != '\0') {
                // No more arguments
                parse_error(/**p +*/ " requires argument");
              } else {
                read_option(option->key, argv[i + 1]);
                return i+2;
              }
            }
          } else {
            parse_error("Unknown short option ''"); // FIXME
          }

          ++p;
        }
      } else {
        read_option(REST_ARG, "-");
      }
    }
  } else {
    read_option(REST_ARG, argv[i]);
  }

  return i + 1;
}

ArgsParse::Option*
ArgsParse::lookup_short_option(char short_option)
{
  for(Options::iterator i = options.begin(); i != options.end(); ++i)
    {
      if (i->key == short_option)
        return &(*i);
    }
  return 0;
}

ArgsParse::Option*
ArgsParse::lookup_long_option(const std::string& long_option)
{
  for(Options::iterator i = options.begin(); i != options.end(); ++i)
    {
      if (i->long_option == long_option)
        return &*i;
    }
  return 0;
}

void
ArgsParse::print_help()
{
  std::cout << "Usage: " << programm << " [OPTIONS]\n" << std::endl;

  std::ostringstream str;

  for(Options::iterator i = options.begin(); i != options.end(); ++i)
    {
      if (i->visible)
        {
          str.str("");

          if (i->key > 255 || i->key < 0)
            str << "--" <<  i->long_option;
          else if (i->long_option.empty())
            str << "-" << char(i->key);
          else
            str << "-" << char(i->key) << ", --" << i->long_option;

          if (!i->argument.empty())
          if (i->long_option.empty())
            str << " " << i->argument;
          else
            str << "=" << i->argument;

          std::cout << "    " 
                    << std::setiosflags( std::ios::left ) << std::setw(18) << str.str() << std::setw(0)
                    << "    " << i->help << std::endl;
        }
    }
  std::cout << std::endl;
}

void
ArgsParse::add_option(int key, 
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

void
ArgsParse::parse_error(const std::string& msg)
{
  std::cout << "Parse Error: " << msg << std::endl;
}

/* EOF */
