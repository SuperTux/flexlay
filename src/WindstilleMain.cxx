//  $Id: WindstilleMain.cxx,v 1.1 2002/03/19 17:56:56 grumbel Exp $
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

//#include <ClanLib/gl.h>
#include <ClanLib/core.h>
#include <ClanLib/jpeg.h>
#include <ClanLib/png.h>
#include <ClanLib/display.h>
#include <SphriteLib/sphritelib.h>

#include <guile/gh.h>

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
SpriteProviderStorage* sprite_storage;

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
  CL_SetupPNG::init ();
  CL_SetupJPEG::init ();

  CL_Display::set_videomode(screen_width, screen_height, 24, 
			    false, // fullscreen 
			    true); // allow resize

  CL_OpenGL::begin_2d();
  glEnable (GL_BLEND);

  try {
    resources =  new CL_ResourceManager ("../data/windstille.scr", false);

    sprite_storage = new SpriteProviderStorage ();
    
    /* Going to do ugly things... */
    sprite_storage->add (new SpriteProvider ("turrican/surround", resources));
    sprite_storage->add (new SpriteProvider ("turrican/walk", resources));
    sprite_storage->add (new SpriteProvider ("turrican/jump", resources));
    sprite_storage->add (new SpriteProvider ("turrican/sit", resources));
    sprite_storage->add (new SpriteProvider ("turrican/stand", resources));
    sprite_storage->add (new SpriteProvider ("turrican/roll", resources));
    sprite_storage->add (new SpriteProvider ("turrican/shild", resources));
    sprite_storage->add (new SpriteProvider ("shoot/bounce", resources));
    sprite_storage->add (new SpriteProvider ("shoot/default", resources));
    sprite_storage->add (new SpriteProvider ("shoot/explosion", resources));
    sprite_storage->add (new SpriteProvider ("dog", resources));
    sprite_storage->add (new SpriteProvider ("bonusflyer", resources));

    sprite_storage->add (new SpriteProvider ("powerup/flash", resources));
    sprite_storage->add (new SpriteProvider ("powerup/laser", resources));
    sprite_storage->add (new SpriteProvider ("powerup/powerup", resources));
    sprite_storage->add (new SpriteProvider ("powerup/spread", resources));
    sprite_storage->add (new SpriteProvider ("powerup/shild", resources));

    sprite_storage->add (new SpriteProvider ("shoot/laser/stage1", resources));
    sprite_storage->add (new SpriteProvider ("shoot/laser/stage2", resources));
    sprite_storage->add (new SpriteProvider ("shoot/laser/stage3", resources));
    sprite_storage->add (new SpriteProvider ("shoot/laser/stage4", resources));
    sprite_storage->add (new SpriteProvider ("shoot/laser/stage5", resources));

    sprite_storage->add (new SpriteProvider ("tiles/green1", resources));
    sprite_storage->add (new SpriteProvider ("tiles/tile1", resources));
    sprite_storage->add (new SpriteProvider ("tiles/tile2", resources));
    sprite_storage->add (new SpriteProvider ("tiles/tile3", resources));
    sprite_storage->add (new SpriteProvider ("tiles/tile4", resources));
    sprite_storage->add (new SpriteProvider ("tiles/tile5", resources));
    sprite_storage->add (new SpriteProvider ("tiles/tile6", resources));
    sprite_storage->add (new SpriteProvider ("tiles/tile7", resources));
    sprite_storage->add (new SpriteProvider ("tiles/tile8", resources));
    sprite_storage->add (new SpriteProvider ("tiles/tile9", resources));

    sprite_storage->add (new SpriteProvider ("tiles/tile10", resources));
    sprite_storage->add (new SpriteProvider ("tiles/tile11", resources));
    sprite_storage->add (new SpriteProvider ("tiles/tile12", resources));
    sprite_storage->add (new SpriteProvider ("tiles/tile13", resources));
    sprite_storage->add (new SpriteProvider ("tiles/tile14", resources));
    sprite_storage->add (new SpriteProvider ("tiles/tile15", resources));
    sprite_storage->add (new SpriteProvider ("tiles/tile16", resources));
    sprite_storage->add (new SpriteProvider ("tiles/tile17", resources));
    sprite_storage->add (new SpriteProvider ("tiles/tile18", resources));
    sprite_storage->add (new SpriteProvider ("tiles/tile19", resources));

    sprite_storage->add (new SpriteProvider ("tiles/tile20", resources));
    sprite_storage->add (new SpriteProvider ("tiles/tile21", resources));
    sprite_storage->add (new SpriteProvider ("tiles/tile22", resources));
    sprite_storage->add (new SpriteProvider ("tiles/tile23", resources));
    sprite_storage->add (new SpriteProvider ("tiles/tile24", resources));
    sprite_storage->add (new SpriteProvider ("tiles/tile25", resources));
    sprite_storage->add (new SpriteProvider ("tiles/tile26", resources));
    sprite_storage->add (new SpriteProvider ("tiles/tile27", resources));
    //sprite_storage->add (new SpriteProvider ("tiles/tile28", resources));
    sprite_storage->add (new SpriteProvider ("tiles/tile29", resources));

    sprite_storage->add (new SpriteProvider ("tiles/tile30", resources));
    sprite_storage->add (new SpriteProvider ("tiles/tile31", resources));
    sprite_storage->add (new SpriteProvider ("tiles/tile32", resources));
    sprite_storage->add (new SpriteProvider ("tiles/tile33", resources));
    sprite_storage->add (new SpriteProvider ("tiles/tile34", resources));
    sprite_storage->add (new SpriteProvider ("tiles/tile35", resources));
    sprite_storage->add (new SpriteProvider ("tiles/tile36", resources));
    sprite_storage->add (new SpriteProvider ("tiles/tile39", resources));

    sprite_storage->add (new SpriteProvider ("tiles/tile40", resources));
    sprite_storage->add (new SpriteProvider ("tiles/tile41", resources));
    sprite_storage->add (new SpriteProvider ("tiles/tile42", resources));
    sprite_storage->add (new SpriteProvider ("tiles/tile43", resources));
    sprite_storage->add (new SpriteProvider ("tiles/tile44", resources));
    sprite_storage->add (new SpriteProvider ("tiles/tile45", resources));
    sprite_storage->add (new SpriteProvider ("tiles/tile46", resources));
    sprite_storage->add (new SpriteProvider ("tiles/tile47", resources));
    sprite_storage->add (new SpriteProvider ("tiles/tile48", resources));
    sprite_storage->add (new SpriteProvider ("tiles/tile49", resources));

    sprite_storage->add (new SpriteProvider ("tiles/tile50", resources));
    sprite_storage->add (new SpriteProvider ("tiles/tile51", resources));
    sprite_storage->add (new SpriteProvider ("tiles/tile52", resources));
    sprite_storage->add (new SpriteProvider ("tiles/tile53", resources));
    sprite_storage->add (new SpriteProvider ("tiles/tile54", resources));
    sprite_storage->add (new SpriteProvider ("tiles/tile55", resources));
    sprite_storage->add (new SpriteProvider ("tiles/tile56", resources));
    sprite_storage->add (new SpriteProvider ("tiles/tile57", resources));
    sprite_storage->add (new SpriteProvider ("tiles/tile58", resources));
    sprite_storage->add (new SpriteProvider ("tiles/tile59", resources));

    sprite_storage->add (new SpriteProvider ("tiles/tile60", resources));
    sprite_storage->add (new SpriteProvider ("tiles/tile61", resources));
    sprite_storage->add (new SpriteProvider ("tiles/tile62", resources));
    sprite_storage->add (new SpriteProvider ("tiles/tile63", resources));
    sprite_storage->add (new SpriteProvider ("tiles/tile64", resources));
    sprite_storage->add (new SpriteProvider ("tiles/tile65", resources));
    sprite_storage->add (new SpriteProvider ("tiles/tile66", resources));
    sprite_storage->add (new SpriteProvider ("tiles/tile67", resources));
    sprite_storage->add (new SpriteProvider ("tiles/tile68", resources));
    sprite_storage->add (new SpriteProvider ("tiles/tile69", resources));

    sprite_storage->add (new SpriteProvider ("tiles/tile71", resources));
    sprite_storage->add (new SpriteProvider ("tiles/tile72", resources));
    sprite_storage->add (new SpriteProvider ("tiles/tile73", resources));
    sprite_storage->add (new SpriteProvider ("tiles/tile75", resources));
    sprite_storage->add (new SpriteProvider ("tiles/tile76", resources));
    sprite_storage->add (new SpriteProvider ("tiles/tile77", resources));

    sprite_storage->add (new SpriteProvider ("lights/light1", resources));
    sprite_storage->add (new SpriteProvider ("lights/light2", resources));
    sprite_storage->add (new SpriteProvider ("lights/light3", resources));

    sprite_storage->add (new SpriteProvider ("nebular", resources));

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

  CL_SetupJPEG::init ();
  CL_SetupPNG::init ();
  CL_SetupDisplay::init();
  CL_SetupGL::init();
  CL_SetupCore::init(); 

  return 0;
}

/* EOF */
