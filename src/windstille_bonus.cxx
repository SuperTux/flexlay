//  $Id: windstille_bonus.cxx,v 1.3 2003/11/06 09:53:43 grumbel Exp $
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

  index = 0;
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
  CL_Display::clear();
  /*CL_Display::clear(CL_Color(int(255 * std::max(0.0f, std::min(1.0f, 1.5f - passed_time/2.0f))),
                             int(255 * std::max(0.0f, std::min(1.0f, 1.5f - passed_time/2.0f))),
                             int(255 * std::max(0.0f, std::min(1.0f, 1.5f - passed_time/2.0f))),
                             255));*/

  sprite.draw(pos.x, pos.y);

  {
    CL_Font font = Fonts::dialog;
    font.set_alignment(origin_top_left);
    font.draw(20, 20, "What's more to come?");
    font.set_alignment(origin_bottom_right);
    font.draw(CL_Display::get_width() - 20,
              CL_Display::get_height() - 20, "You'll never know...");
  }

  CL_Display::flip();
}

void
WindstilleBonus::update(float delta)
{
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
          pos.x = rand()%400 + 200;
          pos.y = rand()%300 + 150;
        }
      else
        {
          quit();
        }
    }

  sprite.set_scale(1.0f + passed_time/3.0f,
                   1.0f + passed_time/3.0f);
  sprite.set_alpha(std::max(0.0f, std::min(1.0f, 1.0f - passed_time/1.85f)));
}

void
WindstilleBonus::on_startup()
{
  MusicManager::current()->play(datadir + "music/Windstille_Ralph_Weinert.ogg", true);
}

void
WindstilleBonus::on_shutdown()
{
  MusicManager::current()->stop();
}

/* EOF */
