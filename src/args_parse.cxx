//  $Id: args_parse.cxx,v 1.1 2003/09/05 20:41:51 grumbel Exp $
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
#include "args_parse.hxx"

ArgsParse::ArgsParse()
{
}

void
ArgsParse::parse_args(int argc, char** argv)
{
  for (int i = 1; i < argc; ++i)
    {
      if (argv[i][0] == '-') {
        if (argv[i][1] == '-') {
          // Long Option
          Option* option = lookup_long_option(argv[i] + 2);
          
          if (option) {
            if (option->argument.empty()) {
              read_option(option->key, "");
            } else {
              if (i == argc - 1)
                {
                  parse_error(std::string(argv[i]) + " requires argument: " + option->argument);
                }
              else
                {
                  read_option(option->key, argv[i + 1]);
                  ++i;
                }
            }
          } else {
            parse_error("Unknown short option '" + std::string(argv[i]) + "'");
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
                  if (i == argc - 1) {
                    // No more arguments
                    parse_error(*p + " requires argument");
                  } else {
                    read_option(option->key, argv[i + 1]);
                    ++i;
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
    }
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
  for(Options::iterator i = options.begin(); i != options.end(); ++i)
    {
      std::cout << "  ";
      if (i->key == NO_SHORT_OPTION)
        std::cout << i->long_option;
      else if (i->long_option.empty())
        if (i->key < 256)
          std::cout << char(i->key);
        else
          std::cout << i->key;
      else
        std::cout << i->key << ", " << i->long_option;

      std::cout << "    " << i->help << std::endl;
    }
  std::cout << std::endl;
}

void
ArgsParse::add_option(int key, 
                      const std::string& long_option, 
                      const std::string& argument,
                      const std::string& help)
{
  Option option;

  option.key          = key;
  option.help         = help;
  option.long_option  = long_option;
  option.argument     = argument;

  options.push_back(option);
}

void
ArgsParse::parse_error(const std::string& msg)
{
  std::cout << "Parse Error: " << msg << std::endl;
}

/* EOF */
