//  $Id$
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

#include <config.h>
#include <iostream>
#include <ClanLib/core.h>
#include <ClanLib/gl.h>
#include <ClanLib/sdl.h>
#include <ClanLib/display.h>
#include <guile/gh.h>
#include "../globals.hxx"
#include "editor.hxx"
#include "tileset.hxx"
#include "editor_main.hxx"

extern "C" void SWIG_init(void);

CL_ResourceManager* resources;

EditorMain::EditorMain()
{
  screen_width  = 800;
  screen_height = 600;
  fullscreen   = false;
  allow_resize = true;
  use_opengl = true;
  game_definition_file = "windstille.scm";
}

EditorMain::~EditorMain()
{
}

void
EditorMain::parse_command_line(int argc, char** argv)
{
  CL_CommandLine argp;

  const int debug_flag  = 256;
  const int game_flag   = 257;
  const int opengl_flag = 258;
  const int sdl_flag = 259;

  argp.set_help_indent(22);
  argp.add_usage ("[OPTION]... [LEVELFILE]...");
  argp.add_doc   ("Windstille Editor - A generic game editor");

  argp.add_group("Display Options:");
  argp.add_option('g', "geometry",   "WxH", "Change window size to WIDTH and HEIGHT");
  argp.add_option('f', "fullscreen", "", "Launch the game in fullscreen");
  argp.add_option(opengl_flag, "opengl", "", "Use OpenGL mode");
  argp.add_option(sdl_flag, "sdl", "", "Use SDL mode");

  argp.add_group("Misc Options:");
  argp.add_option(game_flag, "game", "GAME", "Load the game definition file at startup");
  argp.add_option('d', "datadir",    "DIR", "Fetch game data from DIR");
  argp.add_option(debug_flag, "debug",      "", "Turn on debug output");
  argp.add_option('h', "help",       "", "Print this help");

  argp.parse_args(argc, argv);

  while (argp.next())
    {
      switch (argp.get_key())
        {
        case 'd':
          datadir = argp.get_argument();
          break;

        case debug_flag:
          debug = 1;
          break;

        case game_flag:
          game_definition_file = argp.get_argument();
          break;

        case sdl_flag:
          use_opengl = true;
          break;

        case opengl_flag:
          use_opengl = true;
          break;

        case 'f':
          fullscreen = true;
          break;

        case 'g':
          if (sscanf(argp.get_argument().c_str(), "%dx%d", &screen_width, &screen_height) == 2)
            std::cout << "Geometry: " << screen_width << "x" << screen_height << std::endl;
          else
            throw CL_Error("Geometry option '-g' requires argument of type {WIDTH}x{HEIGHT}");
          break;

        case 'h':
          argp.print_help();
          exit(EXIT_SUCCESS);
          break;

        case CL_CommandLine::REST_ARG:
          levelfiles.push_back(argp.get_argument());
          break;
        }
    }
}

int
EditorMain::main(int argc, char** argv)
{
  try {
    std::cout << "Windstille Editor V0.0" << std::endl;

    parse_command_line(argc, argv);

    // Init the path
    bindir  = CL_System::get_exe_path();

    if (datadir.empty())
      datadir = bindir + "../../data/";

#ifndef WIN32
    char* home_c = getenv("HOME");
    if (home_c) 
      {
        std::string home = home_c; 
        home += "/.windstille";
        if (CL_Directory::create(home))
          std::cout << "Created " << home << std::endl;
        homedir = home + "/";
      }
    else
      {
        throw std::string("Couldn't find environment variable HOME");
      }
#else
    homedir = "config/";
#endif

    // Init Guile
    scm_init_guile();
    SWIG_init();

    std::cout << "Loading Guile Code... " << std::flush;

    gh_eval_str("(debug-enable 'debug)"
                "(debug-enable 'backtrace)"
                "(read-enable 'positions)");

    SCM levelfiles_scm = SCM_EOL;
    for(std::vector<std::string>::iterator i = levelfiles.begin();
        i != levelfiles.end(); ++i)
      {
        levelfiles_scm = gh_cons(gh_str02scm(i->c_str()),
                                 levelfiles_scm);
      }

    gh_define("*windstille-levelfiles*",      gh_reverse(levelfiles_scm));
    gh_define("*windstille-datadir*",        gh_str02scm(datadir.c_str()));
    gh_define("*windstille-homedir*",        gh_str02scm(homedir.c_str()));
    gh_define("*windstille-package-string*", gh_str02scm(PACKAGE_STRING));
    std::cout << "done" << std::endl;

    CL_SetupCore::init();
    if (use_opengl)
      CL_SetupGL::init();
    else
      CL_SetupSDL::init();
    CL_SetupDisplay::init();

    window = new CL_DisplayWindow(PACKAGE_STRING,
                                  screen_width, screen_height, fullscreen, allow_resize);

    resources =  new CL_ResourceManager();
    resources->add_resources(CL_ResourceManager(datadir + "windstille.xml", false));

    std::cout << "Loading Windstille startup script: " << game_definition_file << std::flush;
    gh_load((datadir + game_definition_file).c_str());
    std::cout << "done" << std::endl;

    Editor editor;

    // FIXME: a bit evil interdependency between scripting and C
    for(std::vector<std::string>::iterator i = levelfiles.begin();
        i != levelfiles.end(); ++i)
      gh_call1(gh_lookup("load-map"),
               gh_str02scm(i->c_str()));
        
    editor.run();

    CL_SetupDisplay::deinit();
    if (use_opengl)
      CL_SetupGL::deinit();
    else
      CL_SetupSDL::init();
    CL_SetupCore::deinit();

  } catch (CL_Error& error) {
    std::cout << "CL_Error: " << error.message << std::endl;
  } catch (std::exception& err) {
    std::cout << "std::exception: " << err.what() << std::endl;
  } catch (...) {
    std::cout << "Error catched something unknown?!" << std::endl;
  }

  return 0;
}

/* EOF */
