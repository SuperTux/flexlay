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
#include <ClanLib/Core/core_iostream.h>
#include <ClanLib/display.h>
#include "../globals.hxx"
#include "../tile_factory.hxx"
#include "../tile.hxx"
#include "editor_map.hxx"
#include "src/scripting/editor.hxx"
#include "object_selector.hxx"

ObjectSelector::ObjectSelector(int width, int height, 
                               int obj_w, int obj_h,
                               CL_Component* parent)
  : CL_Component(CL_Rect(CL_Point(0,0), CL_Size(width * obj_w, height * obj_h)),
                 parent),
    width(width), height(height),
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
      release_mouse();
      drag_sprite.set_alpha(1.0f);
      std::cout << "Drag stop: "
                << event.mouse_pos.x + get_screen_rect().left << ", "
                << event.mouse_pos.y + get_screen_rect().top
                << std::endl;

      CL_Point screen(event.mouse_pos.x + get_screen_rect().left,
                      event.mouse_pos.y + get_screen_rect().top);

      CL_Point target(screen.x - EditorMap::current()->get_screen_rect().left,
                      screen.y - EditorMap::current()->get_screen_rect().top);
      
      editor_get_objmap()->add_object(drag_sprite,
                                      EditorMap::current()->screen2world(target),
                                      SCMObj(SCM_BOOL_F));
      drag_sprite = CL_Sprite();
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
        Tile* tile = TileFactory::current()->create(mouse_over_tile);
        if (tile)
          {
            drag_sprite = tile->sur;
            drag_sprite.set_alpha(.5f);
            capture_mouse();
            std::cout << "Drag Start" << std::endl;
          }
      }
      break;
      
    case CL_MOUSE_MIDDLE:
      scrolling = true;
      mouse_pos = event.mouse_pos;
      old_offset = offset;
      capture_mouse();
      break;
      
    case CL_MOUSE_WHEEL_UP:
      offset -= static_cast<int>(TILE_SIZE*scale);
      if (offset < 0)
        offset = 0;
      break;

    case CL_MOUSE_WHEEL_DOWN:
      offset += static_cast<int>(TILE_SIZE*scale);
      break;
    }
}

void
ObjectSelector::mouse_move(const CL_InputEvent& event)
{
  mouse_pos = event.mouse_pos;

  int x = event.mouse_pos.x/static_cast<int>(obj_width);
  int y = (event.mouse_pos.y+offset)/static_cast<int>(obj_height);

  mouse_over_tile = y * width + x;

  if (scrolling)
    {
      offset = old_offset + (mouse_pos.y - event.mouse_pos.y);
      if (offset < 0)
        offset = 0;
    }
}

void 
ObjectSelector::draw()
{
  CL_Display::push_translate_offset(0, -offset);
  for(int y = 0; y < /*height FIXME*/ 40; ++y)
    for(int x = 0; x < width; ++x)
      {
        int i = width * y + x;
        Tile* tile = TileFactory::current()->create(i);

        CL_Rect rect(CL_Point(static_cast<int>(x * obj_width),
                              static_cast<int>(y * obj_height)),
                     CL_Size(static_cast<int>(obj_width),
                             static_cast<int>(obj_height)));

        if (tile)
          {
            CL_Sprite sprite = tile->sur;

            sprite.set_scale((float)obj_width/sprite.get_width(),
                             (float)obj_height/sprite.get_height());

            sprite.draw(static_cast<int>(x * obj_width), 
                        static_cast<int>(y * obj_height));

            CL_Display::draw_rect(rect, CL_Color(0,0,0,128));
          }

        if (mouse_over_tile == i && has_mouse_over())
          {
            CL_Display::fill_rect(rect, CL_Color(0,0,255, 20));
          }
      }
  CL_Display::pop_translate_offset();

  if (drag_sprite)
    {
      CL_Display::set_cliprect(CL_Rect(0, 0, 800, 600));
      drag_sprite.draw(mouse_pos.x, mouse_pos.y);
    }
}

/* EOF */
