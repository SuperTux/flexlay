//  $Id: args_parse_test.cxx,v 1.1 2003/09/05 20:41:51 grumbel Exp $
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

class MyArgsParse : public ArgsParse 
{
public:
  MyArgsParse() {
    add_option('c', "config", "FILE", "Config the app");
    add_option('f', "file",   "FILE", "Load a file");
    add_option('z', "zero",   "",     "Zero Args");
    add_option('a', "",       "",     "short a");
    add_option('h', "help",   "",     "help");
    add_option(500, "arg",    "",     "long a");
  }
  
  void read_option(int key, const std::string& argument) {
    switch (key) {
    case 'h':
      print_help();
      break;
    case 'f':
      std::cout << "file: " << argument << std::endl;
      break;
    default:
      std::cout << "Got " << key << " " << argument << std::endl;
      break;
    }
  }
};

int main(int argc, char** argv)
{
  MyArgsParse args;
  args.parse_args(argc, argv);
}

/* EOF */
