//  $Id: windstille_bonus.cxx,v 1.5 2003/11/07 13:00:39 grumbel Exp $
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

#include <iostream>
#include <algorithm>
#include <ClanLib/Display/display.h>
#include <ClanLib/Display/keyboard.h>
#include <ClanLib/Display/keys.h>
#include <ClanLib/Display/font.h>
#include <list>
#include "fonts.hxx"
#include "globals.hxx"
#include "music_manager.hxx"
#include "windstille_bonus.hxx"

WindstilleBonus::WindstilleBonus()
{
  std::list<std::string> llst = resources->get_all_resources("bonus");
  std::copy(llst.begin(), llst.end(), std::back_inserter(lst));
  std::random_shuffle(lst.begin(), lst.end());

  lst.push_back("bonus_end");

  index = 0;
  state = FADEIN;
  fadeout_value = 0;
  passed_time = 0;
  pos.x = rand()%400 + 200;
  pos.y = rand()%300 + 150;
  sprite = CL_Sprite(lst[index], resources);
  sprite.set_alignment(origin_center);
}

WindstilleBonus::~WindstilleBonus()
{
}
  
void
WindstilleBonus::draw()
{
  //CL_Display::clear();
  CL_Display::fill_rect(CL_Rect(0, 0, CL_Display::get_width(), CL_Display::get_height()),
                        CL_Gradient(CL_Color(0,0,0),
                                    CL_Color(0,0,100),
                                    CL_Color(0,0,100),
                                    CL_Color(0,0,50)));
  {
    CL_Font font = Fonts::dialog;
    font.set_alignment(origin_top_left);
    font.draw(20, 20, "What's to come?");
    font.set_alignment(origin_bottom_right);
    font.draw(CL_Display::get_width() - 20,
              CL_Display::get_height() - 20, "You'll never know...");
  }
  
  if (state == RUNNING || state == FADEOUT)
    sprite.draw(pos.x, pos.y);

  switch (state)
    {
    case FADEOUT:
      CL_Display::fill_rect(CL_Rect(0, 0, 
                                    CL_Display::get_width(), CL_Display::get_height()),
                            CL_Color(0,0,0, std::min(int(fadeout_value*255), 255)));
      break;
    case FADEIN:
      CL_Display::fill_rect(CL_Rect(0, 0, 
                                    CL_Display::get_width(), CL_Display::get_height()),
                            CL_Color(0,0,0, 255-std::min(int(fadeout_value*255), 255)));
      break;
    default:
      break;
    }
  CL_Display::flip();
}

void
WindstilleBonus::update(float delta)
{
  switch (state)
    {
    case FADEIN:
      if (fadeout_value > 1.0f)
        {
          state = RUNNING;
          MusicManager::current()->play(datadir + "music/Windstille_Ralph_Weinert.ogg", true);
        }
      fadeout_value += delta;
      break;

    case FADEOUT:
      if (fadeout_value > 1.0f)
        Screen::quit();

      fadeout_value += delta;
      break;

    case RUNNING:
      if (CL_Keyboard::get_keycode(CL_KEY_ESCAPE))
        quit();

      passed_time += delta;

      if (passed_time > 1.85f)
        {
          index += 1;

          if (index < int(lst.size()))
            {
              passed_time = 0;
              sprite = CL_Sprite(lst[index], resources);
              sprite.set_alignment(origin_center);

              if (index == int(lst.size()) - 1)
                {
                  pos.x = 400;
                  pos.y = 300;
                }
              else
                {
                  pos.x = rand()%400 + 200;
                  pos.y = rand()%300 + 150;
                }
            }
          else
            {
              quit();
            }
        }

      sprite.set_scale(1.0f + passed_time/3.0f,
                       1.0f + passed_time/3.0f);
      sprite.set_alpha(std::max(0.0f, std::min(1.0f, 1.0f - passed_time/1.85f)));
      break;
    }
}

void
WindstilleBonus::quit()
{
  state = FADEOUT;
  fadeout_value = 0;
}

void
WindstilleBonus::on_startup()
{
  MusicManager::current()->stop();
}

void
WindstilleBonus::on_shutdown()
{
  MusicManager::current()->stop();
}

/* EOF */
