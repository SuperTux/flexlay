//  $Id: water_map.cxx,v 1.1 2003/09/12 16:31:21 grumbel Exp $
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
#include "globals.hxx"
#include "water_map.hxx"

WaterMap::WaterMap()
{
  waters.push_back(Water(128/SUBTILE_SIZE, 39,
                         64, 300/SUBTILE_SIZE));
}

WaterMap::~WaterMap()
{
}

void
WaterMap::draw()
{
  for (Waters::iterator i = waters.begin(); i != waters.end(); ++i)
    {
      // Transform subtile CO to pixels
      int x = i->x * SUBTILE_SIZE;
      int y = i->y * SUBTILE_SIZE;
      int w = i->w * SUBTILE_SIZE;
      int h = i->h * SUBTILE_SIZE;

      CL_Display::fill_rect(CL_Rect(x, y, x + w, y + 15), 
                            CL_Gradient(CL_Color(200, 200, 200, 128),
                                        CL_Color(200, 200, 200, 128),
                                        CL_Color(  0,   0, 200, 128),
                                        CL_Color(  0,   0, 200, 128)));

      CL_Display::fill_rect(CL_Rect(x+15, y+15, x + w, y + h - 15), 
                            CL_Gradient(CL_Color(  0,   0, 200, 128),
                                        CL_Color(  0,   0, 200, 128),
                                        CL_Color(  0,   0,  50, 128),
                                        CL_Color(  0,   0,  50, 128)));
    }

  /*
    if (0) // Water
    {
    int x1 = 0;
    int y1 = 100;
    int x2 = 8000;
    int y2 = 5000;
      
    Display::begin_gl();
    {
    glEnable (GL_BLEND);
    glBlendFunc( GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA );
    glBegin(GL_QUADS);
    { // Water
    glColor4f(0.8f, 0.8f, 1.0f, 0.5f);

    glVertex2f(x1, y1);
    glVertex2f(x2, y1);
 
    glColor4f(0.0f, 0.0f, 0.8f, 0.5f);
    glVertex2f(x2, y1 + 15);
    glVertex2f(x1, y1 + 15);

    glVertex2f(x1,  y1 + 15);
    glVertex2f(800, y1 + 15);

    glColor4f(0.0f, 0.0f, .2f, 0.5f);
    glVertex2f(x2, y2);
    glVertex2f(x1, y2);
    }
    glEnd();
    }
    Display::end_gl();
    }*/
}

void
WaterMap::update(float delta)
{
}

/* EOF */
