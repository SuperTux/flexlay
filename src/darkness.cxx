//  $Id: darkness.cxx,v 1.1 2003/09/20 21:55:57 grumbel Exp $
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

#include "darkness.hxx"

void
Darkness::draw()
{
  if (0)
    { // Darkness
      int   vborder = 250;
      int   hborder = 350;
      float left   = hborder;
      float right  = 800 - hborder;
      float top    = vborder;
      float bottom = 600 - vborder;
      float alpha  = .8f;

      Display::begin_gl();
      glEnable (GL_BLEND);
      glBlendFunc( GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA );
      glBegin(GL_QUADS);
        
      glColor4f(0.0f, 0.0f, 0.0f, alpha);
      glVertex2f(0, 0);
      glVertex2f(800, 0);
      glColor4f(0.0f, 0.0f, 0.0f, 0.0f);
      glVertex2f(right, top);
      glVertex2f(left, top);

      glColor4f(0.0f, 0.0f, 0.0f, 0.0f);
      glVertex2f(left, bottom);
      glVertex2f(right, bottom);
      glColor4f(0.0f, 0.0f, 0.0f, alpha);
      glVertex2f(800, 600);
      glVertex2f(  0, 600);

      glColor4f(0.0f, 0.0f, 0.0f, alpha);
      glVertex2f(0, 0);
      glColor4f(0.0f, 0.0f, 0.0f, 0.0f);
      glVertex2f(left, top);
      glVertex2f(left, bottom);
      glColor4f(0.0f, 0.0f, 0.0f, alpha);
      glVertex2f(  0, 600);

      glColor4f(0.0f, 0.0f, 0.0f, 0.0f);
      glVertex2f(right, top);
      glColor4f(0.0f, 0.0f, 0.0f, alpha);
      glVertex2f(800, 0);
      glVertex2f(800, 600);
      glColor4f(0.0f, 0.0f, 0.0f, 0.0f);
      glVertex2f(right, bottom);

      glEnd();
      Display::end_gl();
    }  
}

/* EOF */
