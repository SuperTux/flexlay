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
#include <ClanLib/display.h>
#include <guile/gh.h>
#include "../globals.hxx"
#include "editor.hxx"
#include "tile_factory.hxx"
#include "editor_main.hxx"

extern "C" void SWIG_init(void);

CL_ResourceManager* resources;

EditorMain::EditorMain()
{
  screen_width  = 800;
  screen_height = 600;
  fullscreen   = false;
  allow_resize = false;
}

EditorMain::~EditorMain()
{
}

int
EditorMain::main(int argc, char** argv)
{
  try {
    std::string levelfile;
    std::string game_definition_file = "windstille.scm";
    std::cout << "Windstille Editor V0.0" << std::endl;

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

    gh_define("*windstille-levelfile*",      gh_str02scm(levelfile.c_str()));
    gh_define("*windstille-datadir*",        gh_str02scm(datadir.c_str()));
    gh_define("*windstille-homedir*",        gh_str02scm(homedir.c_str()));
    gh_define("*windstille-package-string*", gh_str02scm(PACKAGE_STRING));
    std::cout << "done" << std::endl;

    CL_SetupCore::init();
    CL_SetupGL::init();
    CL_SetupDisplay::init();

    window = new CL_DisplayWindow(PACKAGE_STRING,
                                  screen_width, screen_height, fullscreen, allow_resize);

    resources =  new CL_ResourceManager();
    resources->add_resources(CL_ResourceManager(datadir + "windstille.xml", false));

    std::cout << "Loading Windstille startup script: " << game_definition_file << std::flush;
    gh_load((datadir + game_definition_file).c_str());
    std::cout << "done" << std::endl;

    TileFactory::init();
    Editor editor;

    if (!levelfile.empty ())
      {
        // FIXME: a bit evil interdependency between scripting and C
        gh_call1(gh_lookup("load-map"),
                 gh_str02scm(levelfile.c_str()));
      }

    editor.run();
    TileFactory::deinit();

    CL_SetupDisplay::deinit();
    CL_SetupGL::deinit();
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
