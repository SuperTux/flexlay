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

#include <iostream>
#include <ClanLib/Core/core_iostream.h>
#include <ClanLib/display.h>
#include "editor_map.hxx"
#include "editor_map_component.hxx"
#include "object_selector.hxx"
#include "object_add_command.hxx"
#include "editor.hxx"

ObjectSelector::ObjectSelector(const CL_Rect& rect, 
                               int obj_w, int obj_h,
                               CL_Component* parent)
  : CL_Component(rect, parent),
    width(rect.get_width()/obj_w), height(rect.get_height()/obj_h),
    obj_width(obj_w), obj_height(obj_h)
{
  index = 0;

  slots.push_back(sig_paint().connect(this, &ObjectSelector::draw));
  slots.push_back(sig_mouse_move().connect(this, &ObjectSelector::mouse_move));
  slots.push_back(sig_mouse_down().connect(this, &ObjectSelector::mouse_down));
  slots.push_back(sig_mouse_up().connect(this, &ObjectSelector::mouse_up));
 
  mouse_over_tile = -1;
  scrolling = false;
  offset = 0;
  scale = 1.0f;
  drag_obj = -1;
}

ObjectSelector::~ObjectSelector()
{
}

void
ObjectSelector::mouse_up(const CL_InputEvent& event)
{
  switch(event.id)
    {
    case CL_MOUSE_LEFT:
      {
        if (drag_obj != -1)
          {
            release_mouse();
      
            if (!has_mouse_over())
              {
                CL_Point screen(event.mouse_pos.x + get_screen_rect().left,
                                event.mouse_pos.y + get_screen_rect().top);

                CL_Point target(screen.x - EditorMapComponent::current()->get_screen_rect().left,
                                screen.y - EditorMapComponent::current()->get_screen_rect().top);
      
                // FIXME: Move this to the scripting layer
                //ObjectAddCommand command(ObjectLayer::current());

                //ObjMapObject obj = brushes[drag_obj].to_sprite_object
                //(EditorMapComponent::current()->screen2world(target)).to_object();

                //command.add_object(obj);
                //Workspace::current().get_map().execute(command.to_command());
                
                //std::cout << "C++: Calling on_drop" << std::endl;
                on_drop(brushes[drag_obj], target);
                //std::cout << "C++: Calling on_drop: done" << std::endl;
              }
            drag_obj = -1;
          }
      }
      break;

    case CL_MOUSE_MIDDLE:
      scrolling = false;
      release_mouse();
      break;

    default:
      break;
    }
}

void
ObjectSelector::mouse_down(const CL_InputEvent& event)
{
  switch(event.id)
    {
    case CL_MOUSE_LEFT:
      {
        if (mouse_over_tile != -1)
          {
            drag_obj = mouse_over_tile;
            capture_mouse();
          }
      }
      break;
      
    case CL_MOUSE_MIDDLE:
      scrolling = true;
      click_pos = event.mouse_pos;
      old_offset = offset;
      capture_mouse();
      break;
      
    case CL_MOUSE_WHEEL_UP:
      offset -= static_cast<int>(obj_height*scale); 
      break;

    case CL_MOUSE_WHEEL_DOWN:
      offset += static_cast<int>(obj_height*scale); 
      break;
    }
}

void
ObjectSelector::mouse_move(const CL_InputEvent& event)
{
  if (scrolling)
    {
      offset = old_offset + (click_pos.y - event.mouse_pos.y);
    }

  mouse_pos = event.mouse_pos;

  int x = (event.mouse_pos.x)/static_cast<int>(obj_width);
  int y = (event.mouse_pos.y+offset)/static_cast<int>(obj_height);

  mouse_over_tile = y * width + x;

  if (mouse_over_tile < 0 || mouse_over_tile >= (int)brushes.size())
    mouse_over_tile = -1;
}

void 
ObjectSelector::draw()
{
  if (offset < 0)
    offset = 0;

  // Handle scrolling in the Component
  CL_Display::push_modelview();
  CL_Display::add_translate(0, -offset);
  CL_Display::add_translate(get_screen_x(), get_screen_y());
    
  for(int i = 0; i < (int)brushes.size(); ++i)
    {
      int x = i%width;
      int y = i/width;

      CL_Rectf rect(CL_Pointf(x * obj_width, y * obj_height),
                    CL_Sizef(obj_width, obj_height));

      CL_Sprite sprite = brushes[i].get_sprite();
      sprite.set_alignment(origin_center, 0, 0);
      sprite.set_scale(std::min(1.0f, (float)obj_width/(float)sprite.get_width()),
                       std::min(1.0f, (float)obj_height/(float)sprite.get_height()));
        
      sprite.draw(x * obj_width + obj_width/2, 
                  y * obj_height + obj_height/2);
        
      //CL_Display::draw_rect(rect, CL_Color(0,0,0,128));
        
      if (mouse_over_tile == i && has_mouse_over())
        {
          CL_Display::fill_rect(rect, CL_Color(0,0,255, 20));
        }
    }

  CL_Display::pop_modelview();

  // Draw drag sprite
  if (drag_obj != -1)
    {
      CL_Display::set_cliprect(CL_Rect(CL_Point(0, 0), 
                                       CL_Size(CL_Display::get_width(),
                                               CL_Display::get_height())));

      CL_Sprite sprite = brushes[drag_obj].get_sprite();
      sprite.set_alpha(0.5f);
      sprite.draw(mouse_pos.x + get_screen_x(), mouse_pos.y + get_screen_y());
    }
}

void
ObjectSelector::add_brush(const ObjectBrush& brush)
{
  brushes.push_back(brush);
}

CL_Signal_v2<ObjectBrush, CL_Point>&
ObjectSelector::sig_drop()
{
  return on_drop;
}

/* EOF */
