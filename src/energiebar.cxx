//  $Id: energiebar.cxx,v 1.1 2003/09/12 22:14:03 grumbel Exp $
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

#include <ClanLib/Display/color.h>
#include <ClanLib/Display/display.h>
#include <ClanLib/Display/display_window.h>
#include <ClanLib/Display/graphic_context.h>
#include "globals.hxx"
#include "energiebar.hxx"

Energiebar::Energiebar()
  : bar("energiebar", resources)
{
}

Energiebar::~Energiebar()
{
}
  
void
Energiebar::draw()
{
  for(int i = 0; i < 15; ++i)
    {
      float red   = 1.0f;
      float green = (i/20.0f);
      CL_Sprite sprite = bar;
      sprite.set_color(red, green, 0, 1.0f);
      sprite.draw(15 + (i * 10), 15);
      CL_Display::get_current_window()->get_gc()->flush();
    }

  for(int i = 15; i < 20; ++i)
    {
      bar.set_color(.5f, .5f, .5f, .5f);
      bar.draw(15 + (i * 10), 15);
    }
}

void
Energiebar::update(float delta)
{
  bar.update(delta);
}

/* EOF */
