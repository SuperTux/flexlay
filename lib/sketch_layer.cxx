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

#include <iostream>
#include <ClanLib/gl.h>
#include <ClanLib/Core/core_iostream.h>
#include <ClanLib/Display/display.h>
#include <ClanLib/Display/sprite.h>
#include <ClanLib/Display/display_window.h>
#include "flexlay.hxx"
#include "layer_impl.hxx"
#include "sketch_layer.hxx"

SketchLayer* SketchLayer::current_ = 0;

void
Stroke::finish()
{
  assert(normals.size() == 0);

  if (points.size() == 1)
    {
      normals.push_back(CL_Pointf(1.0f, 1.0f));
    }
  else if (points.size() == 2)
    {
      normals.push_back(CL_Pointf(1.0f, 1.0f));
      normals.push_back(CL_Pointf(1.0f, 1.0f));
    }
  else if (points.size() >= 3)
    {
      for(Points::size_type i = 0; i < int(points.size())-1; ++i)
        {
          CL_Pointf normal((points[i].y - points[i+1].y),
                           -(points[i].x - points[i+1].x));

          float length = sqrt(normal.x * normal.x + normal.y * normal.y);

          normal.x /= length;
          normal.y /= length;
          
          normals.push_back(normal);
        }
      
      normals.push_back(CL_Pointf(1.0f, 1.0f));
    }

  std::cout << normals.size() << " == " <<  points.size() << std::endl;
  assert(normals.size() == points.size());
}

class SketchLayerImpl : public LayerImpl
{
public:
  typedef std::vector<Stroke> Strokes;
  Strokes strokes;
  CL_Sprite brush;

  SketchLayerImpl() 
    : brush("brush", &(Flexlay::current()->resources))
  {    
  }
  
  void draw(EditorMapComponent* parent) 
  {

    //glLineWidth(3.0);

    for(Strokes::iterator i = strokes.begin(); i != strokes.end(); ++i)
      {
        float spacing = 8.0f;
        brush.set_color(i->color);
        
        if (i->points.size() == 1 || i->points.size() == 2)
          {
            brush.draw((int)i->points.front().x, (int)i->points.front().y);
          }
        else
          {
            float overspace = 0.0f;
            for(unsigned int j = 0; j < i->points.size()-1; ++j)
              {
                CL_Pointf dist = i->points[j+1] - i->points[j];
                float length = sqrt(dist.x * dist.x + dist.y * dist.y);
                int n = 1;

                while (length + overspace > (spacing * n))
                  {
                    float factor = (spacing/length) * n - (overspace/length);
                    CL_Pointf p(i->points[j].x + dist.x * factor,
                                i->points[j].y + dist.y * factor);
                    brush.draw((int)p.x, (int)p.y);
                
                    n += 1;
                  }

                // Space that wasn't used in the last iteration
                overspace = (length + overspace) - (spacing * (n-1));
                //std::cout << "Overspace: " << overspace << std::endl;
              }
          }
      }

    {
      CL_OpenGLState state(CL_Display::get_current_window()->get_gc());
      state.set_active();
      state.setup_2d();

      if (0)
        for(Strokes::iterator i = strokes.begin(); i != strokes.end(); ++i)
          {
            if (i->points.size() >= 2)
              {
                float len = 1.0f;
                float len2 = 2.0f;
            
                glEnable(GL_BLEND);
                glBlendFunc( GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA );

                glBegin(GL_QUAD_STRIP);
                for(unsigned int j = 0; j < i->points.size()-2; ++j)
                  {
                    glColor4ub(i->color.get_red(), i->color.get_green(), i->color.get_blue(), i->color.get_alpha());
                    glVertex2f(i->points[j].x + i->normals[j].x * len,
                               i->points[j].y + i->normals[j].y * len);

                    glColor4ub(i->color.get_red(), i->color.get_green(), i->color.get_blue(), 0);
                    glVertex2f(i->points[j].x + i->normals[j].x * len2,
                               i->points[j].y + i->normals[j].y * len2);
                  }
                glEnd();

                glBegin(GL_QUAD_STRIP);
                for(unsigned int j = 0; j < i->points.size()-2; ++j)
                  {
                    glColor4ub(i->color.get_red(), i->color.get_green(), i->color.get_blue(), 0);
                    glVertex2f(i->points[j].x - i->normals[j].x * len2,
                               i->points[j].y - i->normals[j].y * len2);

                    glColor4ub(i->color.get_red(), i->color.get_green(), i->color.get_blue(), i->color.get_alpha());
                    glVertex2f(i->points[j].x - i->normals[j].x * len,
                               i->points[j].y - i->normals[j].y * len);
                  }
                glEnd();

                glBegin(GL_QUAD_STRIP);
                glColor4ub(i->color.get_red(), i->color.get_green(), i->color.get_blue(), i->color.get_alpha());
                for(unsigned int j = 0; j < i->points.size()-2; ++j)
                  {
                    glVertex2f(i->points[j].x + i->normals[j].x * len,
                               i->points[j].y + i->normals[j].y * len);
                    glVertex2f(i->points[j].x - i->normals[j].x * len,
                               i->points[j].y - i->normals[j].y * len);
                  }
                glEnd();
              }
          }

      if (0)
        for(Strokes::iterator i = strokes.begin(); i != strokes.end(); ++i)
          {
            if (i->points.size() >= 2)
              {
                /*
                  glColor4ub(i->color.get_red(), i->color.get_green(), i->color.get_blue(), i->color.get_alpha());
                  glBegin(GL_LINE_STRIP);
                  for(Stroke::Points::iterator j = i->points.begin(); j != i->points.end(); ++j)
                  glVertex2f(j->x, j->y);
                  glEnd();*/

                float len = 1.0f;
                float len2 = 2.0f;
                glEnable(GL_BLEND);
                glBlendFunc( GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA );
                glBegin(GL_QUADS);
                for(unsigned int j = 0; j < i->points.size()-1; ++j)
                  {
                    glColor4ub(i->color.get_red(), i->color.get_green(), i->color.get_blue(), i->color.get_alpha());
                    glVertex2f(i->points[j].x + i->normals[j].x * len,
                               i->points[j].y + i->normals[j].y * len);
                    glVertex2f(i->points[j].x - i->normals[j].x * len,
                               i->points[j].y - i->normals[j].y * len);
                    glVertex2f(i->points[j+1].x - i->normals[j].x * len,
                               i->points[j+1].y - i->normals[j].y * len);
                    glVertex2f(i->points[j+1].x + i->normals[j].x * len,
                               i->points[j+1].y + i->normals[j].y * len);

                    glColor4ub(i->color.get_red(), i->color.get_green(), i->color.get_blue(), 0);
                    glVertex2f(i->points[j].x + i->normals[j].x * len2,
                               i->points[j].y + i->normals[j].y * len2);

                    glColor4ub(i->color.get_red(), i->color.get_green(), i->color.get_blue(), 0);
                    glVertex2f(i->points[j+1].x + i->normals[j].x * len2,
                               i->points[j+1].y + i->normals[j].y * len2);

                    glColor4ub(i->color.get_red(), i->color.get_green(), i->color.get_blue(), i->color.get_alpha());
                    glVertex2f(i->points[j+1].x + i->normals[j].x * len,
                               i->points[j+1].y + i->normals[j].y * len);

                    glColor4ub(i->color.get_red(), i->color.get_green(), i->color.get_blue(), i->color.get_alpha());
                    glVertex2f(i->points[j].x + i->normals[j].x * len,
                               i->points[j].y + i->normals[j].y * len);

                    /////////////
                    glColor4ub(i->color.get_red(), i->color.get_green(), i->color.get_blue(), i->color.get_alpha());
                    glVertex2f(i->points[j].x - i->normals[j].x * len,
                               i->points[j].y - i->normals[j].y * len);

                    glColor4ub(i->color.get_red(), i->color.get_green(), i->color.get_blue(), 0);
                    glVertex2f(i->points[j].x - i->normals[j].x * len2,
                               i->points[j].y - i->normals[j].y * len2);

                    glColor4ub(i->color.get_red(), i->color.get_green(), i->color.get_blue(), 0);
                    glVertex2f(i->points[j+1].x - i->normals[j].x * len2,
                               i->points[j+1].y - i->normals[j].y * len2);
                    glColor4ub(i->color.get_red(), i->color.get_green(), i->color.get_blue(), i->color.get_alpha());
                    glVertex2f(i->points[j+1].x - i->normals[j].x * len,
                               i->points[j+1].y - i->normals[j].y * len);
                  }
                glEnd();
              }
          }
      //glLineWidth(1.0);
    }
  }

  bool has_bounding_rect() const { 
    return false;
  }
};

SketchLayer::SketchLayer()
  : impl(new SketchLayerImpl())
{
  current_ = this;
}

void
SketchLayer::add_stroke(const Stroke& stroke)
{
  impl->strokes.push_back(stroke);
}

Layer
SketchLayer::to_layer()
{
   return Layer(impl);
}


/* EOF */
