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

#include <ClanLib/Display/display.h>
#include <ClanLib/GUI/component.h>
#include "graphic_context_state.hxx"

GraphicContextState::GraphicContextState(CL_Component* c)
  : comp(c), trans_offset(0,0), zoom(1.0f)
{  
}

void
GraphicContextState::push()
{
  CL_Display::push_modelview();
  CL_Display::add_translate(int(trans_offset.x + comp->get_width()/2), 
                            int(trans_offset.y + comp->get_height()/2));
  //CL_Display::add_scale(get_zoom(), get_zoom());
}

void
GraphicContextState::pop()
{
  CL_Display::pop_modelview();
}

CL_Rect
GraphicContextState::get_clip_rect()
{
  return CL_Rect(CL_Point(int(-trans_offset.x - comp->get_width()/2),
                          int(-trans_offset.y - comp->get_height()/2)),
                 CL_Size(comp->get_width(), comp->get_height()));
}

void
GraphicContextState::set_pos(const CL_Pointf& pos)
{
  trans_offset.x = pos.x;
  trans_offset.y = pos.y;
}

CL_Pointf
GraphicContextState::get_pos()
{
  return trans_offset;
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

CL_Point
GraphicContextState::screen2world(const CL_Point& pos)
{
  int x = int(pos.x - trans_offset.x - comp->get_width()/2);
  int y = int(pos.y - trans_offset.y - comp->get_height()/2);

  return CL_Point(x, y);
}

/* EOF */
