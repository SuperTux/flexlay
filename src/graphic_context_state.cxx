//  $Id$
//
//  Flexlay - A Generic 2D Game Editor
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
#include <ClanLib/GUI/component.h>
#include "graphic_context_state.hxx"

GraphicContextState::GraphicContextState(int w, int h)
  : width(w), height(h), offset(0,0), zoom(1.0f)
{  
}

void
GraphicContextState::push()
{
  CL_Display::push_modelview();
  CL_Display::add_scale(get_zoom(), get_zoom());
  CL_Display::add_translate(offset.x, offset.y);
}

void
GraphicContextState::pop()
{
  CL_Display::pop_modelview();
}

CL_Rect
GraphicContextState::get_clip_rect()
{
  return CL_Rect(CL_Point(int(-offset.x),
                          int(-offset.y)),
                 CL_Size(int(get_width()  / zoom),
                         int(get_height() / zoom)));
}

void
GraphicContextState::set_pos(const CL_Pointf& pos)
{
  offset.x = -pos.x + (get_width()/2  / zoom);
  offset.y = -pos.y + (get_height()/2 / zoom);
}

CL_Pointf
GraphicContextState::get_pos()
{
  return CL_Pointf(-offset.x + (get_width()/2  / zoom),
                   -offset.y + (get_height()/2  / zoom));
}

void
GraphicContextState::set_zoom(CL_Point pos, float z)
{
  float old_zoom = zoom;
  set_zoom(z);
  offset.x = pos.x/zoom - pos.x/old_zoom + offset.x;
  offset.y = pos.y/zoom - pos.y/old_zoom + offset.y;
}

void
GraphicContextState::set_zoom(float z)
{
  zoom = z;
}

float
GraphicContextState::get_zoom()
{
  return zoom;
}

void
GraphicContextState::zoom_to (const CL_Rect& rect)
{
  float center_x = (rect.left + rect.right) / 2.0f;
  float center_y = (rect.top + rect.bottom) / 2.0f;

  float width  = rect.right - rect.left;
  float height = rect.bottom - rect.top;
  float screen_relation = float(get_height())/float(get_width ());
  float rect_relation   = height/width; 
  
  //std::cout << "Screen: " << screen_relation << " Zoom: " << rect_relation << std::endl;
  if (rect_relation < screen_relation) // take width, ignore height
    {
      zoom = get_width()/width; 
    }
  else // take height, ignore width
    {
      zoom = get_height()/height;
    }

  offset.x = (get_width()  / (2*zoom)) - center_x;
  offset.y = (get_height() / (2*zoom)) - center_y;
}

CL_Pointf
GraphicContextState::screen2world(const CL_Point& pos)
{
  return CL_Pointf((pos.x / zoom) - offset.x, 
                   (pos.y / zoom) - offset.y);
}

/* EOF */
