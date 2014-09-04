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

#include "tilemap_select_tool.hpp"

#include "editor_map.hpp"
#include "editor_names.hpp"
#include "globals.hpp"
#include "gui/editor_map_component.hpp"
#include "input_event.hpp"
#include "tile_brush.hpp"
#include "tilemap_layer.hpp"
#include "tool_impl.hpp"

class TileMapSelectToolImpl : public ToolImpl
{
public:
  TileSelection  selection;
  bool creating_selection;

  void draw();

  void on_mouse_up  (const InputEvent& event);
  void on_mouse_down(const InputEvent& event);
  void on_mouse_move(const InputEvent& event);
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
TileMapSelectToolImpl::on_mouse_up(const InputEvent& event)
{
  EditorMapComponent* parent = EditorMapComponent::current();

  switch (event.id)
  {
    case InputEvent::MOUSE_LEFT:
      creating_selection = false;
      parent->release_mouse();

      selection.update(TilemapLayer::current().world2tile(parent->screen2world(event.mouse_pos)));
      break;

   default:
      break;
  }
}

void
TileMapSelectToolImpl::on_mouse_down(const InputEvent& event)
{
  EditorMapComponent* parent = EditorMapComponent::current();

  switch (event.id)
  {
    case InputEvent::MOUSE_LEFT:
    {
      creating_selection = true;
      parent->capture_mouse();
      TilemapLayer tilemap = TilemapLayer::current();
      selection.start(tilemap, tilemap.world2tile(parent->screen2world(event.mouse_pos)));
    }
    break;

    case InputEvent::MOUSE_RIGHT:
      if (!creating_selection)
        selection.clear();
      break;

    default:
      break;
  }
}

void
TileMapSelectToolImpl::on_mouse_move(const InputEvent& event)
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

Rect
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
