//  $Id: windstille_main.cxx,v 1.24 2003/09/30 16:42:26 grumbel Exp $
//
//  Windstille - A Jump'n Shoot Game
//  Copyright (C) 2000 Ingo Ruhnke <grumbel@gmx.de>
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
//#include <ClanLib/gl.h>
#include <ClanLib/core.h>
#include <ClanLib/vorbis.h>
#include <ClanLib/sound.h>
#include <ClanLib/display.h>

#include <guile/gh.h>

#include "globals.hxx"
#include "editor/editor.hxx"
#include "windstille_game.hxx"
#include "guile_gameobj_factory.hxx"
#include "windstille_level.hxx"
#include "windstille_main.hxx"
#include "windstille_menu.hxx"
#include "keyboard_controller.hxx"
#include "fonts.hxx"
#include "tile_factory.hxx"

extern "C" void SWIG_init(void);

CL_ResourceManager* resources;

int 
WindstilleMain::main(int argc, char** argv)
{
  scm_boot_guile (argc, argv,::inner_main, 0);
  return 0;
}

int 
WindstilleMain::inner_main(void* closure, int argc, char** argv)
{
  int  screen_width  =  800;
  int  screen_height = 600;
  bool fullscreen    = false;
  bool allow_resize  = false;

  bool launch_editor = false;
  std::string levelfile;
  
  // Init the path
  bindir  = CL_System::get_exe_path();
  libdir  = bindir + "../lib/";
  datadir = bindir + "../data/";
  
  try {

    CL_CommandLine argp;

    argp.add_usage ("[LEVELFILE]");
    argp.add_doc   ("Windstille is a classic Jump'n Run game.\n");
    argp.add_option('e', "editor",     "", "Launch the level editor");
    argp.add_option('g', "geometry",   "WIDTHxHEIGHT", "Change window size to WIDTH and HEIGHT");
    argp.add_option('f', "fullscreen", "", "Launch the game in fullscreen");
    argp.add_option('h', "help",       "", "Print this help");

    argp.parse_args(argc, argv);

    while (argp.next())
      {
        switch (argp.get_key())
          {
          case 'e':
            launch_editor = true;
            break;
		  
          case 'f':
            fullscreen = true;
            break;

          case 'g':
            if (sscanf(argp.get_argument().c_str(), "%dx%d", &screen_width, &screen_height) == 2)
              {
                std::cout << "Geometry: " << screen_width << "x" << screen_height << std::endl;
              }
            break;
		  
          case 'h':
            argp.print_help();
            return EXIT_SUCCESS;
            break;

          case CL_CommandLine::REST_ARG:
            levelfile = argp.get_argument();
            break;
          }
      }

    SWIG_init();

    CL_SetupCore::init();
    CL_SetupGL::init();
    CL_SetupDisplay::init();
    CL_SetupSound::init();
    CL_SetupVorbis::init();

    CL_DisplayWindow window (PACKAGE_STRING,
                             screen_width, screen_height, fullscreen, allow_resize);
    CL_SoundOutput sound_output(44100);
    CL_Display::clear();
    CL_Display::flip();

    resources =  new CL_ResourceManager();
    resources->add_resources(CL_ResourceManager(datadir + "tiles.xml", false));
    resources->add_resources(CL_ResourceManager(datadir + "windstille.xml", false));

    Fonts::init();
    
    std::cout << "Loading Guile Code..." << std::endl;

    gh_eval_str("(display \"Guile: Enabling debugging...\\n\")"
                "(debug-enable 'debug)"
                "(debug-enable 'backtrace)"
                "(read-enable 'positions)");

    gh_define("*windstille-levelfile*", gh_str02scm(levelfile.c_str()));
    gh_define("*windstille-datadir*", gh_str02scm(datadir.c_str()));
    gh_define("*windstille-package-string*", gh_str02scm(PACKAGE_STRING));

    std::cout << "Loading Guile Code... done" << std::endl;

    // FIXME:
    new KeyboardController();
    TileFactory::init();

    if (!launch_editor && levelfile.empty())
      {
        std::cout << "Starting Menu" << std::endl;
        WindstilleMenu menu;
        menu.display();
      }
    else if (!launch_editor) // Launch Level
      {
        WindstilleGame game (levelfile);
        std::cout << "WindstilleMain: entering main-loop..." << std::endl;
        game.display ();
      }
    else
      {
        Editor editor;
        if (!levelfile.empty ())
          editor.load (levelfile);
        editor.run();
      }
  } catch (CL_Error& error) {
    std::cout << "CL_Error: " << error.message << std::endl;
  }
  
  TileFactory::deinit();
  Fonts::deinit();

  CL_SetupVorbis::init();
  CL_SetupSound::init();
  CL_SetupDisplay::init();
  CL_SetupGL::init();
  CL_SetupCore::init(); 

  return 0;
}

/* EOF */
