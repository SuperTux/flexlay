//  $Id: windstille_game.hxx,v 1.5 2003/09/21 15:22:59 grumbel Exp $
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

#ifndef WINDSTILLEGAME_HXX
#define WINDSTILLEGAME_HXX

#include <string>
#include <ClanLib/Display/sprite.h>
#include <ClanLib/Display/font.h>
#include <ClanLib/Signals/slot_container.h>
#include "screen.hxx"

class Energiebar;
class PlayerView;
class CL_InputEvent;
class GameWorld;
class Background;
class Player;
class DialogManager;

class WindstilleGame : public Screen
{
private:
  Player* player;
  CL_SlotContainer slots;

  float blink;

  std::string filename;
  GameWorld* world;
  PlayerView* view;
  Energiebar* energiebar;
  Background* background;
  DialogManager* dialog_manager;

  enum { DIALOG, GAME } state;

  CL_Font font;
  CL_Sprite portrait;
  CL_Sprite logo;
  CL_Sprite logo_black;

  void on_mouse_up  (const CL_InputEvent& event);
  void on_mouse_down(const CL_InputEvent& event);
  void on_key_down  (const CL_InputEvent& event);

  static WindstilleGame* current_; 
public:
  static WindstilleGame* current() { return current_; } 

  WindstilleGame (const std::string& arg_filename);
  WindstilleGame (GameWorld* w);
  ~WindstilleGame ();

  void set_dialog_state() { state = DIALOG; }
  void set_game_state()   { state = GAME; }

  void on_startup();
  void on_shutdown();

  void draw();
  void update(float delta);
};

#endif

/* EOF */
