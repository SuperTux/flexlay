//  $Id: windstille_main.cxx,v 1.30 2003/11/13 12:59:42 grumbel Exp $
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

#include "string_converter.hxx"
#include "windstille_error.hxx"
#include "globals.hxx"
#include "editor/editor.hxx"
#include "windstille_game.hxx"
#include "guile_gameobj_factory.hxx"
#include "windstille_level.hxx"
#include "windstille_main.hxx"
#include "windstille_menu.hxx"
#include "fonts.hxx"
#include "feuerkraft_error.hxx"
#include "input/input_manager.hxx"
#include "music_manager.hxx"
#include "tile_factory.hxx"

extern "C" void SWIG_init(void);

WindstilleMain main_app;
CL_ResourceManager* resources;

WindstilleMain::WindstilleMain()
{
  screen_width  = 800;
  screen_height = 600;
#ifdef WIN32
  fullscreen    = true;
#else
  fullscreen    = false;
#endif
  allow_resize  = false;
  launch_editor = false;
}

WindstilleMain::~WindstilleMain()
{
}

void
WindstilleMain::parse_command_line(int argc, char** argv)
{
  CL_CommandLine argp;

  const int debug_flag = 256;
    
  argp.set_help_indent(22);
  argp.add_usage ("[LEVELFILE]");
  argp.add_doc   ("Windstille is a classic Jump'n Run game.");

  argp.add_group("Display Options:");
  argp.add_option('g', "geometry",   "WxH", "Change window size to WIDTH and HEIGHT");
  argp.add_option('f', "fullscreen", "", "Launch the game in fullscreen");

  argp.add_group("Sound Options:");
  argp.add_option('s', "disable-sound", "", "Disable sound");
  argp.add_option('S', "enable-sound", "", "Enable sound");

  argp.add_group("Controlls Options:");
  argp.add_option('c', "controller", "FILE", "Use controller as defined in FILE");

  argp.add_group("Misc Options:");
  argp.add_option('e', "editor",     "", "Launch the level editor");
  argp.add_option('d', "datadir",    "DIR", "Fetch game data from DIR");
  argp.add_option(debug_flag, "debug",      "", "Turn on debug output");
  argp.add_option('h', "help",       "", "Print this help");

  argp.add_group("Demo Recording/Playback Options:");
  argp.add_option('r', "record",      "FILE", "Record input events to FILE");
  argp.add_option('a', "record-video","DIR",  "Record a gameplay video to DIR");
  argp.add_option('p', "play",        "FILE", "Playback input events from FILE");

  argp.parse_args(argc, argv);

  while (argp.next())
    {
      switch (argp.get_key())
        {
        case 'r':
          recorder_file = argp.get_argument();
          break;

        case 'a':
          screenshot_dir = argp.get_argument();
          break;

        case 'p':
          playback_file = argp.get_argument();
          break;

        case 'd':
          datadir = argp.get_argument();
          break;

        case debug_flag:
          debug = 1;
          break;

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
        
        case 's':
          sound_disabled = true;
          break;

        case 'S':
          sound_disabled = false;
          break;  

        case 'c':
          controller_file = argp.get_argument();
          break;

        case 'h':
          argp.print_help();
          exit(EXIT_SUCCESS);
          break;

        case CL_CommandLine::REST_ARG:
          levelfile = argp.get_argument();
          break;
        }
    }
}

int 
WindstilleMain::main(int argc, char** argv)
{
#ifdef WIN32
  CL_ConsoleWindow console;
  console.redirect_stdio("windstille.log");
#endif

  // Init the path
  bindir  = CL_System::get_exe_path();

  if (datadir.empty())
    datadir = bindir + "../data/";

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
      throw WindstilleError("Couldn't find environment variable HOME");
    }
#else
  homedir = "config/";
#endif
  
  try {
    parse_command_line(argc, argv);
    init_modules();
    
    std::cout << "Detected " << CL_Joystick::get_device_count() << " joysticks" << std::endl;
        
    if (playback_file.empty())
      {
        if (!controller_file.empty())
          InputManager::init(controller_file);
        else
          InputManager::init(datadir + "controller/keyboard.scm");
      }
    else
      {
        InputManager::init_playback(playback_file);
      }

    if (!recorder_file.empty())
      InputManager::setup_recorder(recorder_file);

    TileFactory::init();
    if (!launch_editor && levelfile.empty())
      {
        if (debug) std::cout << "Starting Menu" << std::endl;
        WindstilleMenu menu;
        menu.display();
      }
    else if (!launch_editor) // Launch Level
      {
        WindstilleGame game (levelfile);
        if (debug) std::cout << "WindstilleMain: entering main-loop..." << std::endl;
        game.display ();
      }
    else
      {
        Editor editor;
        if (!levelfile.empty ())
          editor.load (levelfile);
        editor.run();
      }
    TileFactory::deinit();
    InputManager::deinit();

    deinit_modules();

  } catch (CL_Error& error) {
    std::cout << "CL_Error: " << error.message << std::endl;
  } catch (FeuerkraftError& err) {
    std::cout << "FeuerkraftError: " << err.what() << std::endl;
  } catch (std::exception& err) {
    std::cout << "std::exception: " << err.what() << std::endl;
  } catch (...) {
    std::cout << "Error catched something unknown?!" << std::endl;
  }

  return 0;
}

void
WindstilleMain::init_modules()
{
#ifdef WIN32
  // Make sure that Guile find its files
  // FIXME: this doesn't use 'datadir'
  putenv("GUILE_LOAD_PATH=data\\guile\\");
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
  gh_define("*windstille-package-string*", gh_str02scm(PACKAGE_STRING));

  std::cout << "done" << std::endl;

  // Init ClanLib
  CL_SetupCore::init();
  CL_SetupGL::init();
  CL_SetupDisplay::init();

  if (!sound_disabled)
    {
      CL_SetupSound::init();
      CL_SetupVorbis::init();
    }

  window = new CL_DisplayWindow(PACKAGE_STRING,
                                screen_width, screen_height, fullscreen, allow_resize);
  CL_Display::clear();
  CL_Display::flip();

  if (!sound_disabled)
    sound = new CL_SoundOutput(44100);

  resources =  new CL_ResourceManager();
  resources->add_resources(CL_ResourceManager(datadir + "tiles.xml", false));
  resources->add_resources(CL_ResourceManager(datadir + "windstille.xml", false));

  Fonts::init(); 
  MusicManager::init();
}

void
WindstilleMain::deinit_modules()
{
  MusicManager::deinit();
  Fonts::deinit();

  if (!sound_disabled)
    delete sound;
  
  delete window;

  if (!sound_disabled)
    {
      CL_SetupVorbis::init();
      CL_SetupSound::init();
    }

  CL_SetupDisplay::init();
  CL_SetupGL::init();
  CL_SetupCore::init(); 
}

/* EOF */
