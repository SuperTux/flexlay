//  $Id: player_view.hxx,v 1.3 2003/09/20 21:53:38 grumbel Exp $
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

#ifndef PLAYERVIEW_HXX
#define PLAYERVIEW_HXX

class Player;

class PlayerView
{
private:
  Player*  player;
  GameWorld* world;
  CL_Vector pos;
public:
  PlayerView (Player*);
  
  void draw ();
  void update (float delta);

  CL_Pointf screen2world(CL_Pointf point);
  CL_Pointf world2screen(CL_Pointf point);
};

#endif

/* EOF */
