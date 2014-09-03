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

#include "tilemap_paint_tool.hpp"

#include <ClanLib/Display/keyboard.h>
#include <ClanLib/Display/sprite.h>

#include "display.hpp"
#include "editor_map.hpp"
#include "gui/editor_map_component.hpp"
#include "gui/tile_selection.hpp"
#include "input_event.hpp"
#include "paint_command.hpp"
#include "tile.hpp"
#include "tilemap_layer.hpp"
#include "tileset.hpp"
#include "tool_impl.hpp"

TileMapPaintTool TileMapPaintTool::current_;

class TileMapPaintToolImpl : public ToolImpl
{
public:
  enum { PAINTING, SELECTING, NONE } mode;

  TileSelection selection;
  TileBrush brush;
  Point last_draw;
  Point current_tile;

  PaintCommand* command;

  void draw();

  void on_mouse_down(const InputEvent& event);
  void on_mouse_move(const InputEvent& event);
  void on_mouse_up  (const InputEvent& event);
};

TileMapPaintTool::TileMapPaintTool()
  : impl(new TileMapPaintToolImpl())
{
  impl->last_draw = Point(-1, -1);

  current_  = *this;

  impl->brush = TileBrush(1, 1);
  impl->brush.at(0, 0) = 0;
  impl->brush.set_opaque();
  impl->current_tile = Point(0,0);

  impl->command = 0;

  impl->mode = TileMapPaintToolImpl::NONE;
}

TileMapPaintTool::~TileMapPaintTool()
{
}

void
TileMapPaintToolImpl::draw()
{
  TilemapLayer tilemap = TilemapLayer::current();

  if (tilemap.is_null())
    return;

  switch(mode)
  {
    case TileMapPaintToolImpl::SELECTING:
      if (CL_Keyboard::get_keycode(CL_KEY_LSHIFT))
        selection.draw(Color(255,  128, 128, 100));
      else
        selection.draw();
      break;

    default:
      int tile_size = tilemap.get_tileset().get_tile_size();

      // Draw the brush:
      for(int y = 0; y < brush.get_height(); ++y)
        for(int x = 0; x < brush.get_width(); ++x)
        {
          Tile* tile = tilemap.get_tileset().create(brush.at(x, y));

          if (tile)
          {
            CL_Sprite sprite = tile->get_sprite();
            sprite.set_alpha(0.5f);
            sprite.draw((current_tile.x + x) * tile_size,
                        (current_tile.y + y) * tile_size);

            Display::fill_rect(Rect(Point((current_tile.x + x) * tile_size,
                                             (current_tile.y + y) * tile_size),
                                       Size(tile_size, tile_size)).to_cl(),
                                  Color(255, 255, 255, 100).to_cl());
          }
          else if (brush.is_opaque())
          {
            Display::fill_rect(Rect(Point((current_tile.x + x) * tile_size,
                                             (current_tile.y + y) * tile_size),
                                       Size(tile_size, tile_size)).to_cl(),
                                  Color(255, 255, 255, 100).to_cl());
          }
          else
          {
            Display::fill_rect(Rect(Point((current_tile.x + x) * tile_size,
                                                (current_tile.y + y) * tile_size),
                                       Size(tile_size, tile_size)).to_cl(),
                                  Color(255, 255, 255, 50).to_cl());
          }
        }
      break;
  }
}

const TileBrush&
TileMapPaintTool::get_brush()
{
  return impl->brush;
}

void
TileMapPaintToolImpl::on_mouse_down(const InputEvent& event)
{
  TilemapLayer tilemap = TilemapLayer::current();

  if (!tilemap.is_null())
  {
    EditorMapComponent* parent = EditorMapComponent::current();
    Point pos = tilemap.world2tile(parent->screen2world(event.mouse_pos));

    switch (mode)
    {
      case TileMapPaintToolImpl::NONE:
        switch (event.id)
        {
          case InputEvent::MOUSE_LEFT:
            mode = TileMapPaintToolImpl::PAINTING;
            parent->capture_mouse();
            command = new PaintCommand(tilemap, brush);
            command->add_point(pos);
            last_draw = pos;
            break;

          case InputEvent::MOUSE_RIGHT:
            mode = TileMapPaintToolImpl::SELECTING;
            parent->capture_mouse();

            selection.start(tilemap, pos);
            break;

          default:
            break;
        }
        break;

      default:
        break;
    }
  }
}

void
TileMapPaintToolImpl::on_mouse_move(const InputEvent& event)
{
  TilemapLayer tilemap = TilemapLayer::current();

  if (!tilemap.is_null())
  {
    EditorMapComponent* parent = EditorMapComponent::current();
    current_tile = tilemap.world2tile(parent->screen2world(event.mouse_pos));

    switch (mode)
    {
      case PAINTING:
        if (CL_Keyboard::get_keycode(CL_KEY_LSHIFT) ||
            ((current_tile.x % brush.get_width()) == (last_draw.x % brush.get_width()) &&
             (current_tile.y % brush.get_height() == (last_draw.y % brush.get_height()))))
        {
          command->add_point(current_tile);
          last_draw = current_tile;
        }
        break;

      case SELECTING:
        selection.update(current_tile);
        break;

      default:
        break;
    }
  }
}

void
TileMapPaintToolImpl::on_mouse_up  (const InputEvent& event)
{
  TilemapLayer tilemap = TilemapLayer::current();

  if (!tilemap.is_null())
  {
    EditorMapComponent::current()->get_workspace().get_map().modify();

    EditorMapComponent* parent = EditorMapComponent::current();
    current_tile = tilemap.world2tile(parent->screen2world(event.mouse_pos));

    switch (event.id)
    {
      case InputEvent::MOUSE_LEFT:
        if (mode == PAINTING)
        {
          parent->release_mouse();
          mode = NONE;

          if (CL_Keyboard::get_keycode(CL_KEY_LSHIFT) ||
              ((current_tile.x % brush.get_width()) == (last_draw.x % brush.get_width()) &&
               (current_tile.y % brush.get_height() == (last_draw.y % brush.get_height()))))
          {
            command->add_point(current_tile);
          }

          Workspace::current().get_map().execute(command->to_command());
          command = 0;

          tilemap.draw_tile(brush, current_tile);
          last_draw = Point(-1, -1);
        }
        break;

      case InputEvent::MOUSE_RIGHT:
        if (mode == SELECTING)
        {
          parent->release_mouse();
          mode = NONE;

          selection.update(current_tile);
          brush = selection.get_brush(*tilemap.get_field());

          if ((brush.get_width() > 1 || brush.get_height() > 1)
              && !CL_Keyboard::get_keycode(CL_KEY_LSHIFT))
          {
            brush.set_transparent();
            brush.auto_crop();
          }
          else
          {
            brush.set_opaque();
          }

          selection.clear();
        }
        break;

      default:
        break;
    }
  }
}

void
TileMapPaintTool::set_brush(const TileBrush& b)
{
  impl->brush = b;
}

Tool
TileMapPaintTool::to_tool()
{
  return Tool(impl);
}

/* EOF */
