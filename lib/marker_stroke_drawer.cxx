//  $Id$
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

#include <ClanLib/gl.h>
#include <ClanLib/Display/display.h>
#include "marker_stroke_drawer.hxx"

void
MarkerStrokeDrawer::draw(const Stroke& stroke)
{
  CL_OpenGLState state(CL_Display::get_current_window()->get_gc());
  state.set_active();
  state.setup_2d();

  if (stroke.points.size() >= 2)
    {
      float len  = stroke.get_size()*8.0f;
      float len2 = stroke.get_size()*16.0f;
            
      glEnable(GL_BLEND);
      glBlendFunc( GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA );

      glBegin(GL_QUAD_STRIP);
      for(unsigned int j = 0; j < stroke.points.size()-2; ++j)
        {
          glColor4ub(stroke.color.get_red(), stroke.color.get_green(), stroke.color.get_blue(), stroke.color.get_alpha());
          glVertex2f(stroke.points[j].x + stroke.normals[j].x * len,
                     stroke.points[j].y + stroke.normals[j].y * len);

          glColor4ub(stroke.color.get_red(), stroke.color.get_green(), stroke.color.get_blue(), 0);
          glVertex2f(stroke.points[j].x + stroke.normals[j].x * len2,
                     stroke.points[j].y + stroke.normals[j].y * len2);
        }
      glEnd();

      glBegin(GL_QUAD_STRIP);
      for(unsigned int j = 0; j < stroke.points.size()-2; ++j)
        {
          glColor4ub(stroke.color.get_red(), stroke.color.get_green(), stroke.color.get_blue(), 0);
          glVertex2f(stroke.points[j].x - stroke.normals[j].x * len2,
                     stroke.points[j].y - stroke.normals[j].y * len2);

          glColor4ub(stroke.color.get_red(), stroke.color.get_green(), stroke.color.get_blue(), stroke.color.get_alpha());
          glVertex2f(stroke.points[j].x - stroke.normals[j].x * len,
                     stroke.points[j].y - stroke.normals[j].y * len);
        }
      glEnd();

      glBegin(GL_QUAD_STRIP);
      glColor4ub(stroke.color.get_red(), stroke.color.get_green(), stroke.color.get_blue(), stroke.color.get_alpha());
      for(unsigned int j = 0; j < stroke.points.size()-2; ++j)
        {
          glVertex2f(stroke.points[j].x + stroke.normals[j].x * len,
                     stroke.points[j].y + stroke.normals[j].y * len);
          glVertex2f(stroke.points[j].x - stroke.normals[j].x * len,
                     stroke.points[j].y - stroke.normals[j].y * len);
        }
      glEnd();
    }
}

/* EOF */
