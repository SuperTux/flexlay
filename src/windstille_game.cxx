//  $Id: windstille_game.cxx,v 1.4 2003/08/11 21:50:35 grumbel Exp $
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

#include <ClanLib/gl.h>

#include "game_world.hxx"
#include "gameobj.hxx"
//#include "gamepad_controller.hxx"
#include "keyboard_controller.hxx"
#include "delta_manager.hxx"
#include "player.hxx"
#include "animation_obj.hxx"
#include "tile_map.hxx"
#include "dog.hxx"
#include "power_up.hxx"
#include "bonus_flyer.hxx"
#include "player_view.hxx"

#include "guile_gameobj_factory.hxx"
#include "windstille_game.hxx"

WindstilleGame::WindstilleGame (const std::string& arg_filename)
  : filename (arg_filename)
{
}

void
WindstilleGame::display ()
{ 
  DeltaManager delta_manager;
  
  Controller* controller1;
  Controller* controller2;

#if 0
  if (CL_Input::joysticks.size () >= 2)
    {
      controller1 = new GamepadController (1);
      controller2 = new GamepadController (0);
    }
  else if (CL_Input::joysticks.size () == 1)
    {
      controller1 = new GamepadController (0);
      controller2 = new KeyboardController ();
    }
  else
#endif
    {
      controller1 = new KeyboardController ();
      controller2 = new KeyboardController ();
    }

  Player player1 (controller1);
  player1.set_position (CL_Vector (100, 400));
  player1.set_direction (WEST);

  std::cout << "Creating world" << std::endl;
  GameWorld world (filename);
  GameObj::set_world (&world);

  if (0)
    {
      world.add (new Dog (CL_Vector (320, 200), WEST));
      world.add (new Dog (CL_Vector (320, 200), EAST));

      world.add (new ShildPowerUp (CL_Vector (420, 600)));
      world.add (new ShildPowerUp (CL_Vector (220, 1400)));
      world.add (new ShildPowerUp (CL_Vector (120, 1200)));
      world.add (new SpreadPowerUp (CL_Vector (120, 600)));

      world.add (new BonusFlyer (CL_Vector2 (100, 600)));
    }

  world.add_player (&player1);

  if (0)
    {
      world.add (GuileGameObjFactory::create ("scmdog"));
      world.add (GuileGameObjFactory::create ("scmdog"));
      world.add (GuileGameObjFactory::create ("scmdog"));
      world.add (GuileGameObjFactory::create ("scmdog"));
      world.add (GuileGameObjFactory::create ("scmdog"));
      world.add (GuileGameObjFactory::create ("scmdog"));
      world.add (GuileGameObjFactory::create ("scmdog"));
      world.add (GuileGameObjFactory::create ("scmdog"));
      world.add (GuileGameObjFactory::create ("scmdog"));
      world.add (GuileGameObjFactory::create ("scmdog"));

      world.add (GuileGameObjFactory::create ("scmdog"));
      world.add (GuileGameObjFactory::create ("scmdog"));
      world.add (GuileGameObjFactory::create ("scmdog"));
      world.add (GuileGameObjFactory::create ("scmdog"));
      world.add (GuileGameObjFactory::create ("scmdog"));
      world.add (GuileGameObjFactory::create ("scmdog"));
      world.add (GuileGameObjFactory::create ("scmdog"));
      world.add (GuileGameObjFactory::create ("scmdog"));
      world.add (GuileGameObjFactory::create ("scmdog"));
      world.add (GuileGameObjFactory::create ("scmdog"));
      world.add (GuileGameObjFactory::create ("scmdog"));
      world.add (GuileGameObjFactory::create ("scmdog"));
      world.add (GuileGameObjFactory::create ("scmdog"));

      world.add (GuileGameObjFactory::create ("nebular"));
      world.add (GuileGameObjFactory::create ("nebular"));
      world.add (GuileGameObjFactory::create ("nebular"));
      world.add (GuileGameObjFactory::create ("nebular"));
      world.add (GuileGameObjFactory::create ("nebular"));

      world.add (GuileGameObjFactory::create ("bounce"));
      world.add (GuileGameObjFactory::create ("bounce"));
      world.add (GuileGameObjFactory::create ("bounce"));
      world.add (GuileGameObjFactory::create ("bounce"));
      world.add (GuileGameObjFactory::create ("bounce"));
      world.add (GuileGameObjFactory::create ("bounce"));
      world.add (GuileGameObjFactory::create ("bounce"));
    }

  PlayerView view (&player1);

  while (!CL_Keyboard::get_keycode (CL_KEY_ESCAPE))
    {
      float delta = delta_manager.getset ();
      CL_System::sleep (1);
      //CL_Display::clear();
      CL_Display::begin_3d(); {
        if (0) 
          {
            int dim[4];
            glGetIntegerv(GL_VIEWPORT, dim);
            std::cout << "viewport: " 
                      << dim[0] << "x" << dim[1] << " "
                      << dim[2] << "x" << dim[3] << std::endl;
          }
      
        glMatrixMode(GL_MODELVIEW);
        glLoadIdentity();
        gluOrtho2D (0, 800, 600, 0);
      
        glBlendFunc(GL_ONE, GL_ZERO);

        glBegin(GL_QUADS);
        // Sky
        glColor3f(0.0f, 0.0f, 1.0f);

        glVertex2f(0, 0);
        glVertex2f(800, 0);
      
        glColor3f(0.5f, 0.5f, 1.0f);
        glVertex2f(800,  300);
        glVertex2f(0, 300);

        glVertex2f(0, 300);
        glVertex2f(800,  300);

        glColor3f(1.0f, 1.0f, 1.0f);
        glVertex2f(800,  600);
        glVertex2f(0, 600);
     
        glEnd();
      } CL_Display::end_3d();

      view.draw ();
      view.update (delta);

      world.update (delta);

      CL_Display::begin_3d();
      {
        glMatrixMode(GL_MODELVIEW);
        glLoadIdentity();
        gluOrtho2D (0, 800, 600, 0);

        glEnable (GL_BLEND);
        glBlendFunc( GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA );
        glBegin(GL_QUADS);
        { // Water
          glColor4f(0.8f, 0.8f, 1.0f, 0.5f);

          glVertex2f(0, 500);
          glVertex2f(800, 500);
 
          glColor4f(0.0f, 0.0f, 0.8f, 0.5f);
          glVertex2f(800,  515);
          glVertex2f(0, 515);

          glVertex2f(0, 515);
          glVertex2f(800,  515);

          glColor4f(0.0f, 0.0f, .2f, 0.5f);
          glVertex2f(800,  600);
          glVertex2f(0, 600);
        }
        glEnd();
      }
      CL_Display::end_3d();

      if (0) // Laser
        {
          CL_Display::begin_3d();
          {
            glMatrixMode(GL_MODELVIEW);
            glLoadIdentity();
            gluOrtho2D (0, 800, 600, 0);

            glBlendFunc( GL_SRC_ALPHA, GL_ONE );

            glBegin(GL_QUADS);
            {
              glColor4f(1.0f, 1.0f, 0.0f, 0.9f);
              glVertex2f(150,  400);
              glVertex2f(300, 400);

              glColor4f(1.0f, 1.0f, 0.0f, 0.1f);
              glVertex2f(300, 420);
              glVertex2f(150,  420);
            }
            glEnd();
          }
          CL_Display::end_3d();
        }

      CL_Display::flip ();
	
      //world.add (new AnimationObj ("shoot/explosion", CL_Vector (rand ()% 800, rand ()%600)));

      CL_System::keep_alive ();
    } 
}


/* EOF */
