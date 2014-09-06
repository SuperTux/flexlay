// Flexlay - A Generic 2D Game Editor
// Copyright (C) 2002 Ingo Ruhnke <grumbel@gmail.com>
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <http://www.gnu.org/licenses/>.

#include "objmap_select_tool.hpp"

#include <algorithm>
#include <ClanLib/Display/keyboard.h>

#include "editor_map.hpp"
#include "editor_names.hpp"
#include "graphic_context.hpp"
#include "gui/editor_map_component.hpp"
#include "gui/editor_map_component.hpp"
#include "gui_manager.hpp"
#include "input_event.hpp"
#include "object_delete_command.hpp"
#include "object_move_command.hpp"
#include "objmap_control_point.hpp"
#include "objmap_object.hpp"
#include "popup_menu.hpp"
#include "tool_impl.hpp"

class ObjMapSelectToolImpl : public ToolImpl
{
public:
  boost::signals2::signal<void (CL_Menu*)> on_popup_menu_display;
  boost::signals2::signal<void (int, int)> on_right_click;

  enum { DRAG, SELECT, NONE } state;

  /** the position on which the object was clicked, relative to the
      object */
  Pointf offset;

  Pointf drag_start;
  Rectf selection_rect;

  ObjMapControlPoint control_point;
  ObjMapSelectTool::Selection selection;
  ObjectMoveCommand*   move_command;
  ObjectDeleteCommand* delete_command;

  void draw(GraphicContext& gc);

  void on_mouse_up  (const InputEvent& event);
  void on_mouse_down(const InputEvent& event);
  void on_mouse_move(const InputEvent& event);

  void on_selection_change();
};

ObjMapSelectTool::ObjMapSelectTool()
  : impl(new ObjMapSelectToolImpl())
{
  impl->state = ObjMapSelectToolImpl::NONE;
  impl->offset = Pointf(0, 0);
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

boost::signals2::signal<void (CL_Menu*)>&
ObjMapSelectTool::sig_on_popup_menu_display()
{
  return impl->on_popup_menu_display;
}

boost::signals2::signal<void (int, int)>&
ObjMapSelectTool::sig_on_right_click()
{
  return impl->on_right_click;
}

void
ObjMapSelectToolImpl::draw(GraphicContext& gc)
{
  for (ObjMapSelectTool::Selection::iterator i = selection.begin(); i != selection.end(); ++i)
  {
    //      (*i).draw();
    gc.draw_rect(Rect((*i).get_bound_rect()), Color(255, 0, 0));
  }

  switch(state)
  {
    case DRAG:
      break;
    case SELECT:
      gc.draw_rect(Rect(selection_rect),
                   Color(255, 255, 255));
      break;
    default:
      break;
  }
}

void
ObjMapSelectToolImpl::on_mouse_up(const InputEvent& event)
{
#ifdef GRUMBEL
  ObjectLayer objmap = ObjectLayer::current();

  EditorMapComponent* parent = EditorMapComponent::current();

  Pointf pos = parent->screen2world(event.mouse_pos);

  switch (event.id)
  {
    case InputEvent::MOUSE_LEFT:
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

    case InputEvent::MOUSE_RIGHT:
    {
      on_right_click(event.mouse_pos.x + parent->get_screen_rect().left,
                     event.mouse_pos.y + parent->get_screen_rect().top);
      /*
        PopupMenu* menu = new PopupMenu(Point(event.mouse_pos.x + parent->get_screen_rect().left,
        event.mouse_pos.y + parent->get_screen_rect().top),
        GUIManager::current()->get_component());

        on_popup_menu_display(menu->get_menu());*/
    }
    break;

    default:
      break;
  }
#endif
}

void
ObjMapSelectToolImpl::on_mouse_down(const InputEvent& event)
{
#ifdef GRUMBEL
  ObjectLayer objmap = ObjectLayer::current();

  EditorMapComponent* parent = EditorMapComponent::current();
  Pointf pos = parent->screen2world(event.mouse_pos);

  switch (event.id)
  {
    case InputEvent::MOUSE_LEFT:
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
              selection_rect = Rectf(pos.x, pos.y, pos.x, pos.y);
              parent->capture_mouse();
            }
          }
          break;
      }
      break;

    case InputEvent::MOUSE_RIGHT:
      break;

   default:
      break;
  }
#endif
}

void
ObjMapSelectToolImpl::on_mouse_move(const InputEvent& event)
{
  EditorMapComponent* parent = EditorMapComponent::current();
  Pointf pos = parent->screen2world(event.mouse_pos);

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
