//  $Id: igel.hxx,v 1.2 2003/09/27 20:57:39 grumbel Exp $
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

#ifndef HEADER_IGEL_HXX
#define HEADER_IGEL_HXX

#include <ClanLib/Display/sprite.h>
#include "gameobj.hxx"

/** */
class Igel : public GameObj
{
private:
  CL_Sprite sprite;
  CL_Pointf pos;
  bool direction_left;
  enum { WALKING, FALLING } state;
public:
  Igel(int x, int y);
  virtual ~Igel();

  void draw();
  void update(float delta);
private:
  bool on_ground();
  bool in_wall();

  Igel (const Igel&);
  Igel& operator= (const Igel&);
};

#endif

/* EOF */
