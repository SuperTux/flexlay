//  $Id: player_view.cxx,v 1.5 2003/09/20 21:53:38 grumbel Exp $
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

#include <ClanLib/gl.h>
#include "player.hxx"
#include "player_view.hxx"
#include "game_world.hxx"

PlayerView::PlayerView (Player* t)
  : player (t),
    world (player->get_world ())
{
  
}

void
PlayerView::draw ()
{
  CL_Display::push_translate_offset(int(-pos.x + CL_Display::get_width ()/2),
                                    int(-pos.y + CL_Display::get_height ()/2));
  world->draw ();
  CL_Display::pop_translate_offset();
}

void
PlayerView::update (float delta)
{
  int hscroll_threshold = 100;
  int vscroll_threshold =  50;

  CL_Vector tpos = player->get_pos();

  float dist = tpos.x - pos.x;
  if (dist > hscroll_threshold)
    pos.x = tpos.x - hscroll_threshold;
  else if (dist < - hscroll_threshold)
    pos.x = tpos.x + hscroll_threshold;

  dist = tpos.y - pos.y;
  if (dist > vscroll_threshold)
    pos.y = tpos.y - vscroll_threshold;
  else if (dist < -vscroll_threshold)
    pos.y = tpos.y + vscroll_threshold;
}

CL_Pointf
PlayerView::screen2world(CL_Pointf point)
{
  return CL_Pointf(point.x + pos.x,
                   point.y + pos.y);
}

CL_Pointf
PlayerView::world2screen(CL_Pointf point)
{
  return CL_Pointf(point.x - pos.x,
                   point.y - pos.y);
}

/* EOF */
