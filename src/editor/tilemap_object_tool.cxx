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
#include "editor_map_component.hxx"
#include "editor_map.hxx"
#include "editor_map_component.hxx"
#include "../gui_manager.hxx"
#include "../scm_obj.hxx"
#include "editor_names.hxx"
#include "popup_menu.hxx"
#include "editor.hxx"
#include "objmap_object.hxx"
#include "object_move_command.hxx"
#include "tilemap_object_tool.hxx"

extern CL_ResourceManager* resources;

TileMapObjectTool::TileMapObjectTool()
{
  obj = 0;
  state = NONE;
  offset = CL_Point(0, 0);
  move_command = 0;
}

TileMapObjectTool::~TileMapObjectTool()
{
}
 
void
TileMapObjectTool::draw()
{
  /*if (obj)
    {
      obj->draw();
      CL_Display::draw_rect(obj->get_bound_rect(), CL_Color(255, 255, 0));
    }*/

  for (Selection::iterator i = selection.begin(); i != selection.end(); ++i)
    {
      (*i)->draw();
      CL_Display::draw_rect((*i)->get_bound_rect(), CL_Color(255, 0, 0));
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
  EditorObjMap* objmap = dynamic_cast<EditorObjMap*>(EditorMapComponent::current()->get_map()->get_layer_by_name(OBJECTMAP_NAME));
  EditorMapComponent* parent = EditorMapComponent::current();

  CL_Point pos = parent->screen2world(event.mouse_pos);

  switch (event.id)
    {
    case CL_MOUSE_LEFT:
      switch(state)
        {
        case DRAG:
          if (move_command)
            {
              Editor::current()->execute(move_command);
              move_command = 0;
            }
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
          // FIXME: Evil single click has to go
          obj = objmap->find_object(pos);
          break;
        }
      break;

    case CL_MOUSE_RIGHT:
      {
        PopupMenu* menu = new PopupMenu(CL_Point(event.mouse_pos.x + parent->get_screen_rect().left,
                                                 event.mouse_pos.y + parent->get_screen_rect().top), 
                                        GUIManager::current()->get_component());
        on_popup_menu_display(menu->get_menu());
      }
      break;
    }
}

void
TileMapObjectTool::on_mouse_down(const CL_InputEvent& event)
{
  EditorObjMap* objmap = dynamic_cast<EditorObjMap*>(EditorMapComponent::current()->get_map()->get_layer_by_name(OBJECTMAP_NAME));
  EditorMapComponent* parent = EditorMapComponent::current();
  CL_Point pos = parent->screen2world(event.mouse_pos);
      
  switch (event.id)
    {
    case CL_MOUSE_LEFT:
      switch(state)
        {
        default:
          // FIXME: Evil single object drag has to go
          obj = objmap->find_object(pos);

          if (obj)
            {
              state = DRAG;
              parent->capture_mouse();
              offset = pos - obj->get_pos();
              drag_start = pos;
              Selection::iterator i = std::find(selection.begin(),
                                                selection.end(), 
                                                obj);

              // Clicked object is member of the selection
              if (i != selection.end())
                {
                  obj = 0;
                  move_command = new ObjectMoveCommand(objmap);
                  for (Selection::iterator i = selection.begin(); i != selection.end(); ++i)
                    {
                      move_command->add_obj((*i)->get_handle());
                    }
                }
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
      break;
    }
}

void
TileMapObjectTool::on_mouse_move(const CL_InputEvent& event)
{
  EditorObjMap* objmap = dynamic_cast<EditorObjMap*>(EditorMapComponent::current()->get_map()->get_layer_by_name(OBJECTMAP_NAME));
  EditorMapComponent* parent = EditorMapComponent::current();
  CL_Point pos = parent->screen2world(event.mouse_pos);

  switch(state)
    {
    case DRAG:
      if (obj)
        obj->set_pos(parent->screen2world(event.mouse_pos) - offset);
      else
        {
          for (Selection::iterator i = selection.begin(); i != selection.end(); ++i)
            {
              (*i)->set_pos((*i)->get_pos() + (pos - drag_start));
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

/* EOF */
