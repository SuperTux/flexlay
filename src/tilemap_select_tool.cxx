//  $Id: tilemap_select_tool.cxx,v 1.1 2003/09/23 22:10:40 grumbel Exp $
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
#include <ClanLib/Display/keys.h>
#include <ClanLib/Display/input_event.h>
#include "globals.hxx"
#include "tilemap_layer.hxx"
#include "tool_impl.hxx"
#include "editor_map.hxx"
#include "editor_map_component.hxx"
#include "tile_brush.hxx"
#include "editor_names.hxx"
#include "tilemap_select_tool.hxx"

class TileMapSelectToolImpl : public ToolImpl
{
public:
  TileSelection  selection;
  bool creating_selection;

  void draw();
  
  void on_mouse_up  (const CL_InputEvent& event);
  void on_mouse_down(const CL_InputEvent& event);
  void on_mouse_move(const CL_InputEvent& event);
};

TileMapSelectTool::TileMapSelectTool()
  : impl(new TileMapSelectToolImpl())
{
  impl->creating_selection = false;
}

TileMapSelectTool::~TileMapSelectTool()
{
}

void
TileMapSelectToolImpl::draw()
{
  if (selection.is_active())
    {
      selection.draw();
    }
}

void
TileMapSelectToolImpl::on_mouse_up  (const CL_InputEvent& event)
{
  EditorMapComponent* parent = EditorMapComponent::current();

  switch (event.id)
    {
    case CL_MOUSE_LEFT:
      creating_selection = false;
      parent->release_mouse();

      selection.update(TilemapLayer::current().world2tile(parent->screen2world(event.mouse_pos)));
      break;
    }
}

void
TileMapSelectToolImpl::on_mouse_down(const CL_InputEvent& event)
{
  EditorMapComponent* parent = EditorMapComponent::current();

  switch (event.id)
    {
    case CL_MOUSE_LEFT:
      {
        creating_selection = true;
        parent->capture_mouse();
        TilemapLayer tilemap = TilemapLayer::current();
        selection.start(tilemap, tilemap.world2tile(parent->screen2world(event.mouse_pos)));
      }
      break;
      
    case CL_MOUSE_RIGHT:
      if (!creating_selection)
        selection.clear();
      break;
    }
}

void
TileMapSelectToolImpl::on_mouse_move(const CL_InputEvent& event)
{ 
  EditorMapComponent* parent = EditorMapComponent::current();

  if (creating_selection)
    {
      selection.update(TilemapLayer::current().world2tile(parent->screen2world(event.mouse_pos)));
    }
}

TileBrush
TileMapSelectTool::get_selection() const
{
  TilemapLayer tilemap = TilemapLayer::current();
  return impl->selection.get_brush(*tilemap.get_field());
}

CL_Rect
TileMapSelectTool::get_selection_rect() const
{
  return impl->selection.get_rect();
}

Tool
TileMapSelectTool::to_tool()
{
  return Tool(impl); 
}

/* EOF */
