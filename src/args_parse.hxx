//  $Id: args_parse.hxx,v 1.1 2003/09/05 20:41:51 grumbel Exp $
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

#ifndef HEADER_WINDSTILLE_ARGS_PARSE_HXX
#define HEADER_WINDSTILLE_ARGS_PARSE_HXX

#include <string>
#include <vector>

/** */
class ArgsParse
{
private:
  struct Option 
  {
    int key;
    std::string help;
    std::string long_option;
    std::string argument;
  };
  
  typedef std::vector<Option> Options;
  Options options;
public:
  static const int REST_ARG = -2;
  static const int NO_SHORT_OPTION = -1;

  ArgsParse();
  
  void add_option(int key,
                  const std::string& long_option, 
                  const std::string& argument,
                  const std::string& help);

  void parse_args(int argc, char** argv);
  void print_help();

  virtual void read_option(int id, const std::string& argument) =0;
  virtual void parse_error(const std::string& msg);
private:
  Option* lookup_short_option(char short_option);
  Option* lookup_long_option(const std::string& long_option);

  void parse_option(int argc, char** argv);

  ArgsParse (const ArgsParse&);
  ArgsParse& operator= (const ArgsParse&);
};

#endif

/* EOF */
