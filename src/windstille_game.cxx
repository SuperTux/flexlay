//  $Id: windstille_game.cxx,v 1.16 2003/09/15 17:00:38 grumbel Exp $
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

#include <math.h>
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
#include "display.hxx"
#include "player_view.hxx"
#include "energiebar.hxx"

#include "guile_gameobj_factory.hxx"
#include "windstille_game.hxx"

WindstilleGame::WindstilleGame(const std::string& arg_filename)
  : filename (arg_filename)
{
  world = new GameWorld(filename);
}

WindstilleGame::WindstilleGame(GameWorld* w)
{
  world = w;
}

WindstilleGame::~WindstilleGame()
{
  delete world;
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

  Energiebar energiebar;

  GameObj::set_world (world);

  if (0)
    {
      world->add (new Dog (CL_Vector (320, 200), WEST));
      world->add (new Dog (CL_Vector (320, 200), EAST));

      world->add (new ShildPowerUp (CL_Vector (420, 600)));
      world->add (new ShildPowerUp (CL_Vector (220, 1400)));
      world->add (new ShildPowerUp (CL_Vector (120, 1200)));
      world->add (new SpreadPowerUp (CL_Vector (120, 600)));

      world->add (new BonusFlyer (CL_Vector2 (100, 600)));
    }

  world->add_player (&player1);
  CL_Sprite logo("logo", resources);
  CL_Sprite logo_black("logo_black", resources);
  float blink = 0.0f;
  
  PlayerView view (&player1);

  gh_load ((datadir + "game.scm").c_str());

  bool pause = false;
  while (!CL_Keyboard::get_keycode (CL_KEY_ESCAPE))
    {
      float delta = delta_manager.getset ();
      CL_System::sleep (1);
      
      Display::begin_gl();
      {
        glBlendFunc(GL_ONE, GL_ZERO);

        glBegin(GL_QUADS);
        // Sky
        glColor3f(0.0f, 0.0f, 0.2f);

        glVertex2f(0, 0);
        glVertex2f(800, 0);
      
        glColor3f(0.3f, 0.3f, .5f);
        glVertex2f(800,  300);
        glVertex2f(0, 300);

        glVertex2f(0, 300);
        glVertex2f(800,  300);

        glColor3f(.0f, .0f, .0f);
        glVertex2f(800,  600);
        glVertex2f(0, 600);
     
        glEnd();
      }
      Display::end_gl();

      view.draw ();
      
      if (CL_Keyboard::get_keycode (CL_KEY_P))
        {
          pause = !pause;
          while (CL_Keyboard::get_keycode (CL_KEY_P))
            CL_System::keep_alive();
        }

      if (!pause)
        {
          float step = 20/1000.0f;
          float d = delta;
          while (d > step)
            {
              view.update (step);
              world->update (step);
              energiebar.update(step);
              d -= step;
            }
          view.update (d);
          world->update (d);
          energiebar.update(d);
        }

      if (0) // Laser
        {
          Display::begin_gl();
          
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

          Display::end_gl();
        }
      
      if (0)
        { // Darkness
          int   vborder = 250;
          int   hborder = 350;
          float left   = hborder;
          float right  = 800 - hborder;
          float top    = vborder;
          float bottom = 600 - vborder;
          float alpha  = .8f;

          Display::begin_gl();
          glEnable (GL_BLEND);
          glBlendFunc( GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA );
          glBegin(GL_QUADS);
        
          glColor4f(0.0f, 0.0f, 0.0f, alpha);
          glVertex2f(0, 0);
          glVertex2f(800, 0);
          glColor4f(0.0f, 0.0f, 0.0f, 0.0f);
          glVertex2f(right, top);
          glVertex2f(left, top);

          glColor4f(0.0f, 0.0f, 0.0f, 0.0f);
          glVertex2f(left, bottom);
          glVertex2f(right, bottom);
          glColor4f(0.0f, 0.0f, 0.0f, alpha);
          glVertex2f(800, 600);
          glVertex2f(  0, 600);

          glColor4f(0.0f, 0.0f, 0.0f, alpha);
          glVertex2f(0, 0);
          glColor4f(0.0f, 0.0f, 0.0f, 0.0f);
          glVertex2f(left, top);
          glVertex2f(left, bottom);
          glColor4f(0.0f, 0.0f, 0.0f, alpha);
          glVertex2f(  0, 600);

          glColor4f(0.0f, 0.0f, 0.0f, 0.0f);
          glVertex2f(right, top);
          glColor4f(0.0f, 0.0f, 0.0f, alpha);
          glVertex2f(800, 0);
          glVertex2f(800, 600);
          glColor4f(0.0f, 0.0f, 0.0f, 0.0f);
          glVertex2f(right, bottom);

          glEnd();
          Display::end_gl();
        }

      if (1)
        {
          blink += delta * 3.141f;
      
          //logo.set_blend_func(blend_src_alpha, blend_one);
          logo.set_alpha(sin(blink)*0.5f + 0.5f);
          logo.draw(800 - 302, 600 - 95);
          logo_black.draw(800 - 302, 600 - 95);
        }

      energiebar.draw();
      CL_Display::flip();
	
      //world->add (new AnimationObj ("shoot/explosion", CL_Vector (rand ()% 800, rand ()%600)));

      CL_System::keep_alive ();
    } 
}

/* EOF */
