//  $Id: igel.hxx,v 1.4 2003/11/05 11:08:31 grumbel Exp $
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

#ifndef HEADER_IGEL_HXX
#define HEADER_IGEL_HXX

#include <ClanLib/Display/sprite.h>
#include "gameobj.hxx"

/** */
class Igel : public GameObj
{
private:
  CL_Sprite sprite;
  CL_Sprite die_sprite;

  CL_Pointf pos;
  bool direction_left;
  enum { WALKING, FALLING, DIEING } state;
public:
  Igel(int x, int y);
  virtual ~Igel();

  void draw();
  void update(float delta);
  void die();
  CL_Pointf get_pos() { return pos; }
private:
  bool on_ground();
  bool in_wall();

  Igel (const Igel&);
  Igel& operator= (const Igel&);
};

#endif

/* EOF */
