//  $Id$
// 
//  Flexlay - A Generic 2D Game Editor
//  Copyright (C) 2004 Ingo Ruhnke <grumbel@gmx.de>
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

#include <ClanLib/Core/Math/point.h>
#include <ClanLib/Display/display.h>
#include "workspace.hxx"
#include "tool_impl.hxx"
#include "editor_map_component.hxx"
#include "editor_map.hxx"
#include "layer.hxx"
#include "layer_move_tool.hxx"

class LayerMoveToolImpl : public ToolImpl
{
public:
  bool scrolling;
  CL_Pointf click_pos;

  /** Position of the center */
  CL_Pointf old_trans_offset;
  Layer layer;
  
  Layer find_closed_layer(const CL_Pointf& pos)
  {
    Layer layer; 

    EditorMap parent = EditorMapComponent::current()->get_workspace().get_map();

    for(int i = 0; i < parent.get_layer_count(); ++i)
      {
        if (parent.get_layer(i).get_bounding_rect().is_inside(CL_Point(pos)))
          layer = parent.get_layer(i);
      }

    return layer;
  }

  void draw() 
  {
    for(int i = 0; i < EditorMapComponent::current()->get_workspace().get_map().get_layer_count(); ++i)
      {
        Layer layer = EditorMapComponent::current()->get_workspace().get_map().get_layer(i);
        if (layer.has_bounding_rect())
          {
            CL_Rect rect = layer.get_bounding_rect();
            CL_Display::draw_line(rect.left, rect.top, rect.right, rect.bottom,
                                  CL_Color(0, 255, 255));
            CL_Display::draw_line(rect.left, rect.bottom, rect.right, rect.top,
                                  CL_Color(0, 255, 255));
          }
      }
  }

  void on_mouse_up  (const CL_InputEvent& event) 
  {
    if (!layer.is_null())
      {
        scrolling = false;
        update(event);
        EditorMapComponent::current()->release_mouse();
        layer = Layer();
      }
  }

  void on_mouse_down(const CL_InputEvent& event)
  {
    EditorMapComponent* parent = EditorMapComponent::current();
    CL_Pointf pos = parent->screen2world(event.mouse_pos);

    layer = find_closed_layer(pos);
    if (!layer.is_null())
      {
        scrolling = true;
        old_trans_offset = layer.get_pos();
        click_pos = pos;
        EditorMapComponent::current()->capture_mouse();
      }
  }

  void on_mouse_move(const CL_InputEvent& event)
  {
    if (!layer.is_null())
      {
        if (scrolling)
          {
            update(event);
          }
      }    
  }

  void update(const CL_InputEvent& event)
  {
    if (!layer.is_null())
      {
        EditorMapComponent* parent = EditorMapComponent::current();
        CL_Pointf pos = parent->screen2world(event.mouse_pos);
        layer.set_pos(old_trans_offset + (pos - click_pos));
      }
  }
};

LayerMoveTool::LayerMoveTool()
  : impl(new LayerMoveToolImpl())
{
  impl->scrolling = false;
  impl->click_pos = CL_Point(0, 0);
  impl->old_trans_offset = CL_Pointf(0,0);
}

Tool
LayerMoveTool::to_tool()
{
  return Tool(impl);
}

/* EOF */

