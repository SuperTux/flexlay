//  $Id: dialog_manager.cxx,v 1.3 2003/09/30 16:42:26 grumbel Exp $
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
#include "input/controller.hxx"
#include "input/input_manager.hxx"
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
  current_choice = 0;
}

void
DialogManager::add_dialog(const std::string& portrait, const std::string& text)
{
  dialogs.push_back(Dialog(portrait, text));
  current_dialog = 0;
}

void
DialogManager::add_answer(const std::string& text, SCMFunctor func)
{
  Dialog& dialog = dialogs[current_dialog];
  dialog.answers.push_back(std::pair<std::string, SCMFunctor>(text, func));
}

void
DialogManager::draw()
{
  if (current_dialog != -1)
    {
      Dialog& dialog = dialogs[current_dialog];
      
      CL_Rect text_rect = Fonts::dialog.bounding_rect(CL_Rect(CL_Point(260, 0), CL_Size(420, 600)),
                                                      dialog.text);

      CL_Display::fill_rect(CL_Rect(CL_Point(100, 100), CL_Size(600, text_rect.get_height() + 80)), 
                            CL_Gradient(CL_Color(0,0,0,228),
                                        CL_Color(0,0,0,228),
                                        CL_Color(0,0,0,128),
                                        CL_Color(0,0,0,128)));
      CL_Display::draw_rect(CL_Rect(CL_Point(100, 100), CL_Size(600, text_rect.get_height() + 80)), 
                            CL_Color(255,255,255, 80));
      CL_Display::flush();
      
      CL_Display::fill_rect(CL_Rect(CL_Point(120, 120), CL_Size(120, 120)),
                            CL_Gradient(CL_Color(100,100,100,68), CL_Color(100,100,100,68),
                                        CL_Color(255,255,255, 255), CL_Color(255,255,255, 255)));
      
      dialog.portrait.draw(120, 120);

      Fonts::dialog.set_alignment(origin_top_left);
      Fonts::dialog_h.set_alignment(origin_top_left);

      Fonts::dialog.draw(CL_Rect(CL_Point(260, 120), CL_Size(420, 0)),
                         dialog.text);
      //Fonts::dialog.set_alignment(origin_bottom_right);    
      //Fonts::dialog.draw(680, 290, ">>>");

      Fonts::dialog.set_alignment(origin_top_center);
      Fonts::dialog_h.set_alignment(origin_top_center);

      if (dialog.answers.size() > 0)
        {
          int w = (500/dialog.answers.size());
          for(Dialogs::size_type i = 0; i < dialog.answers.size(); ++i)
            {
              if (int(i) == current_choice)
                Fonts::dialog_h.draw(150 + i*w + w/2, 120 + text_rect.get_height() + 20,
                                     "[" + dialog.answers[i].first + "]");
              else
                Fonts::dialog.draw(150 + i*w + w/2, 120 + text_rect.get_height() + 20,
                                   dialog.answers[i].first);
            }
        }
    }
}

void
DialogManager::update(float delta)
{
  if (current_dialog != -1)
    {
      InputEventLst events = InputManager::get_controller().get_events();

      for (InputEventLst::iterator i = events.begin(); i != events.end(); ++i)
        {
          if ((*i).type == BUTTON_EVENT)
            {
          if ((*i).button.name == FIRE_BUTTON && (*i).button.down == true)
            {
              WindstilleGame::current()->set_game_state();
              if (dialogs[current_dialog].answers.size() > 0)
                dialogs[current_dialog].answers[current_choice].second();
            }
          else if ((*i).button.name == LEFT_BUTTON && (*i).button.down == true)
            {
              current_choice -= 1;
            }
          else if ((*i).button.name == RIGHT_BUTTON && (*i).button.down == true)
            {
              current_choice += 1;
            }
            }
        }

      if (current_choice < 0)
        current_choice = 0;
      else if (current_choice >= int(dialogs[current_dialog].answers.size()))
        current_choice = int(dialogs[current_dialog].answers.size()) - 1;
    }
  else
    {
      std::cout << "DialogManager: No dialog available" << std::endl;
      WindstilleGame::current()->set_game_state();
    }
}

void
DialogManager::clear()
{
  current_dialog = 0;
  current_choice = 0;
  dialogs.clear();
}

/* EOF */
