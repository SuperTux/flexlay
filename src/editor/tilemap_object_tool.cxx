//  $Id: tilemap_object_tool.cxx,v 1.1 2003/09/23 22:10:40 grumbel Exp $
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

#include <algorithm>
#include <ClanLib/Display/keys.h>
#include <ClanLib/Display/display.h>
#include "editor_map.hxx"
#include "tilemap_object_tool.hxx"

extern CL_ResourceManager* resources;

TileMapObjectTool::TileMapObjectTool(EditorMap* p, EditorObjMap* t)
  : TileMapTool(p), objmap(t)
{
  obj = 0;
  state = NONE;
  offset = CL_Point(0, 0);

  brush.set_sprite(CL_Sprite("igel", resources));
}

TileMapObjectTool::~TileMapObjectTool()
{
}
 
void
TileMapObjectTool::draw()
{
  if (obj)
    {
      CL_Sprite sprite = obj->sprite;
      sprite.set_color(CL_Color(255, 0, 255));
      sprite.draw(obj->pos.x, obj->pos.y);
    }

  for (Selection::iterator i = selection.begin(); i != selection.end(); ++i)
    {
      CL_Sprite sprite = (*i)->sprite;
      sprite.set_color(CL_Color(255, 0, 0));
      sprite.draw((*i)->pos.x, (*i)->pos.y);      
    }

  { // Draw Brush    
    CL_Sprite b(brush.get_sprite());
    b.set_alpha(0.25f);
    b.draw(brush_pos.x, brush_pos.y);
  }

  switch(state)
    {
    case DRAG:
      break;
    case SELECT:
      CL_Display::draw_rect(selection_rect, CL_Color(255, 255, 255));
      break;
    default:
      break;
    }
}

void
TileMapObjectTool::on_mouse_up(const CL_InputEvent& event)
{
  CL_Point pos = parent->screen2world(event.mouse_pos);

  switch (event.id)
    {
    case CL_MOUSE_LEFT:
      switch(state)
        {
        case DRAG:
          state = NONE;
          parent->release_mouse();
          break;

        case SELECT:
          state = NONE;
          selection_rect.right  = pos.x;
          selection_rect.bottom = pos.y;
          selection_rect.normalize();

          selection = objmap->get_selection(selection_rect);
          parent->release_mouse();
          break;

        default:
          obj = objmap->find_object(pos);
          break;
        }
      break;

    case CL_MOUSE_RIGHT:
      break;
    }
}

void
TileMapObjectTool::on_mouse_down(const CL_InputEvent& event)
{
  CL_Point pos = parent->screen2world(event.mouse_pos);
      
  switch (event.id)
    {
    case CL_MOUSE_LEFT:
      switch(state)
        {
        default:
          obj = objmap->find_object(pos);

          if (obj)
            {
              state = DRAG;
              parent->capture_mouse();
              offset = pos - obj->pos;
              drag_start = pos;
              Selection::iterator i = std::find(selection.begin(),
                                                selection.end(), 
                                                obj);

              // Clicked object is member of the selection
              if (i != selection.end())
                obj = 0;
            }
          else
            {
              state = SELECT;
              selection_rect = CL_Rect(pos.x, pos.y, pos.x, pos.y);
              parent->capture_mouse();
            }
          break;
        }
      break;

    case CL_MOUSE_RIGHT:
      objmap->add_object(brush.get_sprite(), pos);
      break;
    }
}

void
TileMapObjectTool::on_mouse_move(const CL_InputEvent& event)
{
  CL_Point pos = parent->screen2world(event.mouse_pos);
  brush_pos = pos;

  switch(state)
    {
    case DRAG:
      if (obj)
        obj->pos = parent->screen2world(event.mouse_pos) - offset;
      else
        {
          for (Selection::iterator i = selection.begin(); i != selection.end(); ++i)
            {
              (*i)->pos += (pos - drag_start);
            }
          drag_start = pos;
        }
      break;

    case SELECT:
      selection_rect.right  = pos.x;
      selection_rect.bottom = pos.y;
      break;

    default:
      CL_Point pos = parent->screen2world(event.mouse_pos);
      obj = objmap->find_object(pos);
      break;
    }
}

void
TileMapObjectTool::set_brush(const std::string& name)
{
  brush.set_sprite(CL_Sprite(name, resources));
}

/* EOF */
