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
#include "stroke_drawer_impl.hxx"
#include "stroke.hxx"
#include "drawer_properties.hxx"
#include "marker_stroke_drawer.hxx"

class MarkerStrokeDrawerImpl : public StrokeDrawerImpl
{
public:
  MarkerStrokeDrawerImpl() {}

  void draw(const Stroke& stroke, CL_GraphicContext* gc)
  {
    CL_OpenGLState state(CL_Display::get_current_window()->get_gc());
    state.set_active();
    state.setup_2d();

    CL_Color color = DrawerProperties::current()->get_color();

    const Stroke::Dabs& dabs = stroke.get_interpolated_dabs(DrawerProperties::current()->get_spacing(),
                                                            DrawerProperties::current()->get_spacing());

    if (dabs.size() >= 2)
      {
        std::vector<CL_Pointf> normals;
        
        if (stroke.get_dab_count() == 2)
          {
            normals.push_back(CL_Pointf(1.0f, 1.0f));
            normals.push_back(CL_Pointf(1.0f, 1.0f));
          }
        else if (stroke.get_dab_count() >= 3)
          {
            for(Stroke::Dabs::size_type i = 0; i < dabs.size()-1; ++i)
              {
                CL_Pointf normal((dabs[i].pos.y - dabs[i+1].pos.y),
                                 -(dabs[i].pos.x - dabs[i+1].pos.x));

                float length = sqrt(normal.x * normal.x + normal.y * normal.y);

                normal.x /= length;
                normal.y /= length;
          
                normals.push_back(normal);
              }
      
            normals.push_back(CL_Pointf(1.0f, 1.0f));
          }

        float len  = DrawerProperties::current()->get_size() * 8.0f;
        float len2 = DrawerProperties::current()->get_size() * 16.0f;
            
        glEnable(GL_BLEND);
        glBlendFunc( GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA );

        glBegin(GL_QUAD_STRIP);
        for(unsigned int j = 0; j < dabs.size()-2; ++j)
          {
            glColor4ub(color.get_red(), color.get_green(), color.get_blue(), color.get_alpha());
            glVertex2f(dabs[j].pos.x + normals[j].x * len,
                       dabs[j].pos.y + normals[j].y * len);

            glColor4ub(color.get_red(), color.get_green(), color.get_blue(), 0);
            glVertex2f(dabs[j].pos.x + normals[j].x * len2,
                       dabs[j].pos.y + normals[j].y * len2);
          }
        glEnd();

        glBegin(GL_QUAD_STRIP);
        for(unsigned int j = 0; j < dabs.size()-2; ++j)
          {
            glColor4ub(color.get_red(), color.get_green(), color.get_blue(), 0);
            glVertex2f(dabs[j].pos.x - normals[j].x * len2,
                       dabs[j].pos.y - normals[j].y * len2);

            glColor4ub(color.get_red(), color.get_green(), color.get_blue(), color.get_alpha());
            glVertex2f(dabs[j].pos.x - normals[j].x * len,
                       dabs[j].pos.y - normals[j].y * len);
          }
        glEnd();

        glBegin(GL_QUAD_STRIP);
        glColor4ub(color.get_red(), color.get_green(), color.get_blue(), color.get_alpha());
        for(unsigned int j = 0; j < dabs.size()-2; ++j)
          {
            glVertex2f(dabs[j].pos.x + normals[j].x * len,
                       dabs[j].pos.y + normals[j].y * len);
            glVertex2f(dabs[j].pos.x - normals[j].x * len,
                       dabs[j].pos.y - normals[j].y * len);
          }
        glEnd();
      }
  }

  StrokeDrawerImpl* clone() const 
  {
    MarkerStrokeDrawerImpl* drawer = new MarkerStrokeDrawerImpl();
  
    *drawer = *this;
    
    return drawer;
  }
};

MarkerStrokeDrawer::MarkerStrokeDrawer()
  : impl(new MarkerStrokeDrawerImpl())
{
}

StrokeDrawer
MarkerStrokeDrawer::to_drawer()
{
  return StrokeDrawer(impl);
}

/* EOF */
