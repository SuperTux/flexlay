//  $Id: windstille_menu.hxx,v 1.3 2003/09/29 21:26:46 grumbel Exp $
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

#ifndef HEADER_WINDESTILLE_MENU_HXX
#define HEADER_WINDESTILLE_MENU_HXX

#include <ClanLib/Display/sprite.h>
#include "screen.hxx"

/** */
class WindstilleMenu : public Screen
{
private:
  CL_Sprite background;
  CL_Sprite windstille;
  int current_choice;
  float passed_time;
public:
  WindstilleMenu();
  ~WindstilleMenu();
  
  void update(float delta);
  void draw();
private:
  void fadeout();
  WindstilleMenu (const WindstilleMenu&);
  WindstilleMenu& operator= (const WindstilleMenu&);
};

#endif

/* EOF */
