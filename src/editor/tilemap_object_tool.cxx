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

#include "editor_map.hxx"
#include "tilemap_object_tool.hxx"

TileMapObjectTool::TileMapObjectTool(EditorMap* p, EditorObjMap* t)
  : TileMapTool(p), objmap(t)
{
  obj = 0;
  state = NONE;
  offset = CL_Point(0, 0);
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
      sprite.set_color(CL_Color(0, 0, 255));
      sprite.draw(obj->pos.x, obj->pos.y);
    }
}

void
TileMapObjectTool::on_mouse_up(const CL_InputEvent& event)
{
  switch(state)
    {
    case DRAG:
      state = NONE;
      break;
    default:
      CL_Point pos = parent->screen2world(event.mouse_pos);
      obj = objmap->find_object(pos);
    }
}

void
TileMapObjectTool::on_mouse_down(const CL_InputEvent& event)
{
  switch(state)
    {
    default:
      CL_Point pos = parent->screen2world(event.mouse_pos);
      obj = objmap->find_object(pos);
      if (obj)
        {
          state = DRAG;
          offset = pos - obj->pos;
        }
    }
}

void
TileMapObjectTool::on_mouse_move(const CL_InputEvent& event)
{
  switch(state)
    {
    case DRAG:
      obj->pos = parent->screen2world(event.mouse_pos) - offset;
      break;
    default:
      CL_Point pos = parent->screen2world(event.mouse_pos);
      obj = objmap->find_object(pos);
      break;
    }
}

/* EOF */
