//  $Id: WindstilleMain.cxx,v 1.4 2003/08/06 17:29:19 grumbel Exp $
//
//  Pingus - A free Lemmings clone
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
#include <ClanLib/display.h>

#include <guile/gh.h>

#include "globals.hxx"
#include "editor/WindstilleEditor.hxx"
#include "WindstilleGame.hxx"
#include "SpriteSmob.hxx"
#include "GameObjSmob.hxx"
#include "TileMapSmob.hxx"
#include "GameWorldSmob.hxx"
#include "GuileGameObjFactory.hxx"
#include "WindstilleLevel.hxx"
#include "WindstilleMain.hxx"

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
  int screen_width =  800;
  int screen_height = 600;

  CL_SetupCore::init();
  CL_SetupGL::init();
  CL_SetupDisplay::init();

  CL_DisplayWindow window (PACKAGE_STRING,
                           screen_width, screen_height);

  //CL_OpenGL::begin_2d();
  //glEnable (GL_BLEND);

  try {
    resources =  new CL_ResourceManager ("../data/windstille.xml", false);
    
    std::cout << "Loading Guile Code..." << std::endl;

    // Debuging on
    SCM_DEVAL_P = 1;
    SCM_BACKTRACE_P = 1;
    SCM_RECORD_POSITIONS_P = 1;
    SCM_RESET_DEBUG_MODE;

    SpriteSmob::register_guile_bindings ();    
    GameObjSmob::register_guile_bindings ();    
    GameWorldSmob::register_guile_bindings ();    
    TileMapSmob::register_guile_bindings ();    
    GuileGameObjFactory::register_guile_bindings ();    
    gh_load ("guile/windstille.scm");
    std::cout << "Loading Guile Code... done" << std::endl;

    bool launch_editor = false;
    std::string filename;
    for (int i = 1; i < argc; ++i)
      {
	if (strcmp (argv[i], "--editor") == 0)
	  {
	    launch_editor = true;
	  }
	else
	  {
	    //std::cout << "Unknown argument: " << argv[i] << std::endl;
	    //exit (EXIT_FAILURE);
	    filename = argv[i];
	  }
      }

    if (!launch_editor)
      {
	if (filename.empty ())
	  filename = "../data/levels/level1.xml";
	WindstilleGame game (filename);
	std::cout << "Launching game..." << std::endl;
	game.display ();
      }
    else
      {
	WindstilleEditor editor;
	if (!filename.empty ())
	  editor.load (filename);
	editor.display ();
      }
  } catch (CL_Error& error) {
    std::cout << "CL_Error: " << error.message << std::endl;
  }

  CL_SetupDisplay::init();
  CL_SetupGL::init();
  CL_SetupCore::init(); 

  return 0;
}

/* EOF */
