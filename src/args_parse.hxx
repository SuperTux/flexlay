//  -*- mode: clanlib -*-
//  $Id: args_parse.hxx,v 1.6 2003/09/07 21:01:45 grumbel Exp $
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

class CL_ArgsParse_Generic;

/** The CL_ArgsParse class helps to parse command line arguments, namely
 *  the argc/argv pair that you get from main(). CL_ArgsParse mimiks
 *  getopt_long() behaviour as closly as possible, while providing a
 *  cleaner interface and a few additional feature, like automatic
 *  generation of '--help' output. CL_ArgsParse can parse long arguments
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
 *  To avoid ambiguity when a filename starts with '-' CL_ArgsParse stops
 *  parsing arguments after the first encounter of a '--', so in
 *
 *  programm -f -b -- -f -b
 *
 *  In the above example the first '-f -b' options are treated as
 *  normal while the ones after the '--' are treated as rest arguments
 *  (aka filenames in most programms).
 *
 *  To use CL_ArgsParse you implement a class that derives from CL_ArgsParse
 *  and implement the virtual read_option() function and add all
 *  supported options to with the add_option() function.
 *
 */
class CL_ArgsParse
{
public:
	static const int REST_ARG  = -2;

	CL_ArgsParse();
	~CL_ArgsParse();

	/** Change the indention used for the help message, default is 18 */
	void set_help_indent(int i);

	/** Add a usage line to the help output, usage is for example
	 * "[OPTIONS]... FILE", the programm name will get appended
	 * automatically */
	void add_usage(const std::string& usage);

	/** Adds extra documentation to the help output, should only be
	 * used at the beginning or at the end, to */
	void add_doc(const std::string& doc);

	/** Starts a new group of options, the \a grouptopic gets printed
	 *  above the group of options in the print_help() output */
	void add_group(const std::string& grouptopic);
  
	/** Adds a option to the parser
	 *
	 *  @param key a letter for a short-option or a numeric value
	 *  larger than 255 that identifies the option
	 *
	 *  @param long_option the long name of this option or "" if non
	 *  should be used for this option
	 *
	 *  @param argument the type of the argument that this option
	 *  requires (i.e. FILE, SIZE, WIDTH, etc.) or "" if no argument is
	 *  required
	 *
	 *  @param help the help string for this option
	 *
	 *  @param visible true if the option should be listed in the help
	 *  output, false will not list it in the help output which might
	 *  be usefull for cheat-options or backward-compability options
	 *  than would only clutter the help output
	 */
	void add_option(int key,
						 const std::string& long_option, 
						 const std::string& argument,
						 const std::string& help,
						 bool visible = true);
	
	/** Parse the options arcording to the options added with
	 * add_option(), result of the parsing is accessible via
	 * begin()/end()
	 */
	void parse_args(int argc, char** argv);

	/** Print the help output, normaly done via a --help option
	 */
	void print_help();
	
	bool next();

	int get_key();
	std::string get_argument();
private:
	CL_ArgsParse_Generic* impl;
};

#endif

/* EOF */
