//  $Id: args_parse.hxx,v 1.5 2003/09/06 17:29:07 grumbel Exp $
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

/** The ArgsParse class helps to parse command line arguments, namely
 *  the argc/argv pair that you get from main(). ArgsParse mimiks
 *  getopt_long() behaviour as closly as possible, while providing a
 *  cleaner interface and a few additional feature, like automatic
 *  generation of '--help' output. ArgsParse can parse long arguments
 *  in the following styles:
 *
 *  programm --file FILENAME
 *  programm --file=FILENAME
 *
 *  Short arguments are handled like this:
 *
 *  programm -f FILENAME
 *
 *  Concatenating short arguments is also supported, so that:
 *  
 *  programm -f -a -b FILENAME
 *
 *  is equivalent to:
 *
 *  programm -fab FILENAME
 *
 *  Non-option arguments (aka rest arguments) are supported as well:
 *
 *  programm SOMEFILE SOMEOTHERFILE ...
 * 
 *  To avoid ambiguity when a filename starts with '-' ArgsParse stops
 *  parsing arguments after the first encounter of a '--', so in
 *
 *  programm -f -b -- -f -b
 *
 *  In the above example the first '-f -b' options are treated as
 *  normal while the ones after the '--' are treated as rest arguments
 *  (aka filenames in most programms).
 *
 *  To use ArgsParse you implement a class that derives from ArgsParse
 *  and implement the virtual read_option() function and add all
 *  supported options to with the add_option() function.
 *
 */
class ArgsParse
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
  struct ParsedOption {
    int key;
    std::string argument;
  };
private:
  typedef std::vector<ParsedOption> ParsedOptions;
  ParsedOptions parsed_options;

public:  
  typedef  ParsedOptions::iterator iterator;
  iterator begin() { return parsed_options.begin(); }
  iterator end()   { return parsed_options.end(); }

  static const int REST_ARG  = -2;
  static const int GROUP     = -3;
  static const int DOC       = -4;
  static const int USAGE     = -5;
  static const int NO_SHORT_OPTION = -1;

  ArgsParse();

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

  ArgsParse (const ArgsParse&);
  ArgsParse& operator= (const ArgsParse&);
};

#endif

/* EOF */
