//  $Id: dialog_manager.cxx,v 1.1 2003/09/21 17:34:54 grumbel Exp $
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

#include <ClanLib/Display/display.h>
#include <iostream>
#include "fonts.hxx"
#include "windstille_game.hxx"
#include "controller.hxx"
#include "dialog_manager.hxx"

DialogManager* DialogManager::current_ = 0;

Dialog::Dialog(const std::string& portrait, const std::string& text)
  : portrait(portrait, resources), text(text)
{
}

DialogManager::DialogManager()
{
  current_ = this;
  current_dialog = -1;
}

void
DialogManager::add_dialog(const std::string& portrait, const std::string& text)
{
  dialogs.push_back(Dialog(portrait, text));
  current_dialog = 0;
}

void
DialogManager::draw()
{
  if (current_dialog != -1)
    {
      Dialog& dialog = dialogs[current_dialog];
      
      CL_Display::fill_rect(CL_Rect(CL_Point(100, 100), CL_Size(600, 200)), CL_Color(0,0,0,128));
      CL_Display::draw_rect(CL_Rect(CL_Point(100, 100), CL_Size(600, 200)), CL_Color(255,255,255, 80));
      CL_Display::flush();
      
      CL_Display::fill_rect(CL_Rect(CL_Point(120, 120), CL_Size(120, 120)),
                            CL_Gradient(CL_Color(100,100,100,68), CL_Color(100,100,100,68),
                                        CL_Color(255,255,255, 255), CL_Color(255,255,255, 255)));
      
      dialog.portrait.draw(120, 120);

      Fonts::dialog.set_alignment(origin_top_left);
      Fonts::dialog.draw(CL_Rect(CL_Point(260, 120), CL_Size(420, 0)),
                         dialog.text);
      Fonts::dialog.set_alignment(origin_bottom_right);
      Fonts::dialog.draw(680, 290, ">>>");
    }
}

void
DialogManager::update(float delta)
{
  Controller::Events& events = Controller::current()->get_events();

  for (Controller::Events::iterator i = events.begin();
       i != events.end(); ++i)
    {
      if ((*i).type == InputEvent::FIRE && (*i).state == true)
        WindstilleGame::current()->set_game_state();
    }
}

void
DialogManager::clear()
{
  dialogs.clear();
}

/* EOF */
