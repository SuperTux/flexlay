//  $Id: windstille_game.cxx,v 1.24 2003/09/29 21:51:40 grumbel Exp $
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
#include "player.hxx"
#include "animation_obj.hxx"
#include "tile_map.hxx"
#include "dog.hxx"
#include "power_up.hxx"
#include "bonus_flyer.hxx"
#include "display.hxx"
#include "player_view.hxx"
#include "energiebar.hxx"
#include "background.hxx"
#include "dialog_manager.hxx"

#include "guile_gameobj_factory.hxx"
#include "windstille_game.hxx"

WindstilleGame* WindstilleGame::current_ = 0; 

WindstilleGame::WindstilleGame(const std::string& arg_filename)
  : filename (arg_filename)
{
  current_ = this;
  world = new GameWorld(filename);
  state = GAME;
}

WindstilleGame::WindstilleGame(GameWorld* w)
{
  current_ = this;
  world = w;
  state = GAME;
}

WindstilleGame::~WindstilleGame()
{
  delete world;
}

void
WindstilleGame::on_mouse_up(const CL_InputEvent& event)
{
  CL_Pointf pos = view->screen2world(CL_Pointf(event.mouse_pos.x,
                                               event.mouse_pos.y));
  gh_call2(gh_lookup("*mouse-up-handler*"), 
           gh_double2scm(pos.x), gh_double2scm(pos.y));
}

void
WindstilleGame::on_mouse_down(const CL_InputEvent& event)
{
  CL_Pointf pos = view->screen2world(CL_Pointf(event.mouse_pos.x,
                                               event.mouse_pos.y));
  gh_call2(gh_lookup("*mouse-down-handler*"), 
           gh_double2scm(pos.x), gh_double2scm(pos.y));
}

void
WindstilleGame::on_key_down(const CL_InputEvent& event)
{
  gh_call1(gh_lookup("*key-down-handler*"), 
           gh_str02scm(CL_Keyboard::get_device().keyid_to_string(event.id).c_str()));
}

void
WindstilleGame::draw_game()
{
  background->draw();

  // Draw the world
  view->draw ();

  // Draw HUD
  energiebar->draw();

  if (state == DIALOG)
    {
      dialog_manager->draw();      
    }

  // Draw Logo
  if (1)
    {     
      //logo.set_blend_func(blend_src_alpha, blend_one);
      logo.set_alpha(cos(blink)*0.5f + 0.5f);
      logo.draw(800 - 302, 600 - 95);
      logo_black.draw(800 - 302, 600 - 95);
    }
}

void
WindstilleGame::draw()
{
  draw_game();
  CL_Display::flip();
}

void
WindstilleGame::update(float delta)
{
  delta *= game_speed;

  view->update (delta);

  if (state == DIALOG)
    {
      dialog_manager->update(delta);
    }
  else if (state == GAME)
    {
      world->update (delta);
      energiebar->update(delta);
    }
  Controller::current()->clear();

  blink += delta * 3.141f;
}    

void
WindstilleGame::on_startup ()
{ 
  blink = 0.0f;

  GameObj::set_world (world);
  
  player = new Player(Controller::current());
  view   = new PlayerView(player);
  
  energiebar = new Energiebar();
  background = new Background();
  dialog_manager = new DialogManager();

  world->add_player(player);

  gh_load((datadir + "game.scm").c_str());

  logo       = CL_Sprite("logo", resources);
  portrait   = CL_Sprite("hero/portrait", resources);
  logo_black = CL_Sprite("logo_black", resources);

  slots.connect(CL_Mouse::sig_key_down(), this, &WindstilleGame::on_mouse_down);
  slots.connect(CL_Mouse::sig_key_up(), this, &WindstilleGame::on_mouse_up);
  slots.connect(CL_Keyboard::sig_key_down(), this, &WindstilleGame::on_key_down);

  world->on_startup();
}

void
WindstilleGame::on_shutdown ()
{
  delete energiebar;
  delete background;
  delete view;
  delete dialog_manager;
}

void
WindstilleGame::quit()
{
  fadeout();
  Screen::quit();
}

void
WindstilleGame::fadeout()
{
  int alpha = 0;
  while (alpha <= 255)
    {
      draw_game();
      CL_Display::fill_rect(CL_Rect(0, 0, 
                                    CL_Display::get_width(), CL_Display::get_height()),
                            CL_Color(0,0,0, std::min(alpha, 255)));
      CL_Display::flip();
      CL_System::keep_alive();
      CL_System::sleep(50);
      alpha += 15;
    }
}

/* EOF */
