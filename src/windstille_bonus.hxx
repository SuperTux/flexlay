//  $Id: windstille_bonus.hxx,v 1.1 2003/11/05 22:44:49 grumbel Exp $
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

#ifndef HEADER_WINDSTILLE_BONUS_HXX
#define HEADER_WINDSTILLE_BONUS_HXX

#include <vector>
#include <ClanLib/Display/sprite.h>
#include "screen.hxx"

/** */
class WindstilleBonus : public Windstille::Screen
{
private:
  float passed_time;
  int index;
  CL_Sprite sprite;
  CL_Point pos;

  typedef std::vector<std::string> Names;
  Names lst;
public:
  WindstilleBonus();
  ~WindstilleBonus();
  
  void draw();
  void update(float delta);

  void on_startup();
  void on_shutdown();
};

#endif

/* EOF */
