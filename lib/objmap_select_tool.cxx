//  $Id: tilemap_object_tool.cxx,v 1.1 2003/09/23 22:10:40 grumbel Exp $
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

#include <algorithm>
#include <ClanLib/Display/keys.h>
#include <ClanLib/Display/keyboard.h>
#include <ClanLib/Display/display.h>
#include "editor_map_component.hxx"
#include "editor_map.hxx"
#include "editor_map_component.hxx"
#include "gui_manager.hxx"
#include "editor_names.hxx"
#include "popup_menu.hxx"
#include "editor.hxx"
#include "objmap_object.hxx"
#include "objmap_control_point.hxx"
#include "object_move_command.hxx"
#include "object_delete_command.hxx"
#include "tool_impl.hxx"
#include "objmap_select_tool.hxx"

class ObjMapSelectToolImpl : public ToolImpl
{
public:
  CL_Signal_v1<CL_Menu*> on_popup_menu_display;
  CL_Signal_v2<int, int> on_right_click;

  enum { DRAG, SELECT, NONE } state;

  /** the position on which the object was clicked, relative to the
      object */
  CL_Pointf offset;

  CL_Pointf drag_start;
  CL_Rectf selection_rect;

  ObjMapControlPoint control_point;
  ObjMapSelectTool::Selection selection;
  ObjectMoveCommand*   move_command;
  ObjectDeleteCommand* delete_command;

  void draw();

  void on_mouse_up  (const CL_InputEvent& event);
  void on_mouse_down(const CL_InputEvent& event);
  void on_mouse_move(const CL_InputEvent& event);

  void on_selection_change();
};

ObjMapSelectTool::ObjMapSelectTool()
  : impl(new ObjMapSelectToolImpl())
{
  impl->state = ObjMapSelectToolImpl::NONE;
  impl->offset = CL_Pointf(0, 0);
  impl->move_command = 0;
}

ObjMapSelectTool::~ObjMapSelectTool()
{
}
 
void
ObjMapSelectTool::clear_selection()
{
  impl->selection.clear(); 
  impl->on_selection_change();
}

ObjMapSelectTool::Selection
ObjMapSelectTool::get_selection() const 
{ 
  return impl->selection;
}

void
ObjMapSelectTool::set_selection(const Selection& sel) 
{ 
  impl->selection = sel; 
}

CL_Signal_v1<CL_Menu*>& 
ObjMapSelectTool::sig_on_popup_menu_display()
{
  return impl->on_popup_menu_display; 
}

CL_Signal_v2<int, int>&
ObjMapSelectTool::sig_on_right_click()
{
  return impl->on_right_click;
}

void
ObjMapSelectToolImpl::draw()
{
  for (ObjMapSelectTool::Selection::iterator i = selection.begin(); i != selection.end(); ++i)
    {
      //      (*i).draw();
      CL_Display::draw_rect((*i).get_bound_rect(), CL_Color(255, 0, 0));
    }

  switch(state)
    {
    case DRAG:
      break;
    case SELECT:
      CL_Display::draw_rect(selection_rect,
                            CL_Color(255, 255, 255));
      break;
    default:
      break;
    }
}

void
ObjMapSelectToolImpl::on_mouse_up(const CL_InputEvent& event)
{
  ObjectLayer objmap = ObjectLayer::current();

  EditorMapComponent* parent = EditorMapComponent::current();

  CL_Pointf pos = parent->screen2world(event.mouse_pos);

  switch (event.id)
    {
    case CL_MOUSE_LEFT:
      switch(state)
        {
        case DRAG:
          if (move_command)
            {
              Workspace::current().get_map().execute(move_command->to_command());
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

          selection = objmap.get_selection(selection_rect);
          on_selection_change();
          parent->release_mouse();
          break;

        default:
          break;
        }
      break;

    case CL_MOUSE_RIGHT:
      {
        on_right_click(event.mouse_pos.x + parent->get_screen_rect().left,
                       event.mouse_pos.y + parent->get_screen_rect().top);
        /*
        PopupMenu* menu = new PopupMenu(CL_Point(event.mouse_pos.x + parent->get_screen_rect().left,
                                                 event.mouse_pos.y + parent->get_screen_rect().top), 
                                        GUIManager::current()->get_component());

                                        on_popup_menu_display(menu->get_menu());*/
      }
      break;
    }
}

void
ObjMapSelectToolImpl::on_mouse_down(const CL_InputEvent& event)
{
  ObjectLayer objmap = ObjectLayer::current();

  EditorMapComponent* parent = EditorMapComponent::current();
  CL_Pointf pos = parent->screen2world(event.mouse_pos);
      
  switch (event.id)
    {
    case CL_MOUSE_LEFT:
      switch(state)
        {
        default:
          control_point = objmap.find_control_point(pos);

          if (!control_point.is_null())
            {
              state = DRAG;
              parent->capture_mouse();
              offset = pos - control_point.get_pos();
              drag_start = pos;
            }
          else
            {
              ObjMapObject obj = objmap.find_object(pos);

              if (!obj.is_null())
                {
                  if (CL_Keyboard::get_keycode(CL_KEY_LSHIFT))
                    {
                      ObjMapSelectTool::Selection::iterator i
                        = std::find(selection.begin(), selection.end(), obj);
                      if (i == selection.end())
                        selection.push_back(obj);
                      else
                        selection.erase(i);

                      on_selection_change();
                    }
                  else
                    {
                      state = DRAG;
                      parent->capture_mouse();
                      offset = pos - obj.get_pos();
                      drag_start = pos;

                      if (std::find(selection.begin(), selection.end(), obj) == selection.end())
                        { // Clicked object is not in the selection, so we add it
                          selection.clear();
                          objmap.delete_control_points();
                          selection.push_back(obj);
                          on_selection_change();
                        }

                      move_command = new ObjectMoveCommand(objmap);
                      for (ObjMapSelectTool::Selection::iterator i = selection.begin();
                           i != selection.end(); ++i)
                        {
                          move_command->add_obj(*i);
                        }
                    }
                }
              else
                {
                  state = SELECT;
                  selection_rect = CL_Rectf(pos.x, pos.y, pos.x, pos.y);
                  parent->capture_mouse();
                }
            }
          break;
        }
      break;

    case CL_MOUSE_RIGHT:
      break;
    }
}

void
ObjMapSelectToolImpl::on_mouse_move(const CL_InputEvent& event)
{
  EditorMapComponent* parent = EditorMapComponent::current();
  CL_Pointf pos = parent->screen2world(event.mouse_pos);

  switch(state)
    {
    case DRAG:
      if (!control_point.is_null())
        {
          control_point.set_pos(pos - offset);
        }
      else
        {
          move_command->move_by(pos - drag_start);
          if (selection.size() == 1)
            selection.front().update_control_points();
        }
      /*
      for (ObjMapSelectTool::Selection::iterator i = selection.begin(); 
           i != selection.end(); ++i)
        {
          (*i).set_pos((*i).get_pos() + (pos - drag_start));
          // FIXME: Move this into ObjMapObject
          (*i).sig_move()(*i);
        }*/
      //drag_start = pos;
      break;

    case SELECT:
      selection_rect.right  = pos.x;
      selection_rect.bottom = pos.y;
      break;

    default:
      // FIXME: Add some kind of highlighting here if mouse is over an object
      break;
    }
}

Tool
ObjMapSelectTool::to_tool()
{
  return Tool(impl); 
}

void
ObjMapSelectToolImpl::on_selection_change()
{
  ObjectLayer objmap = ObjectLayer::current();
  objmap.delete_control_points();

  if (selection.size() == 1)
    {
      selection.front().add_control_points();
    } 
}

/* EOF */
