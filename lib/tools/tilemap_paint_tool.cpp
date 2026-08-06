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
#include <ClanLib/Display/keys.h>
#include <ClanLib/Display/display.h>
#include <ClanLib/Display/sprite.h>

#include "tilemap_layer.hpp"
#include "tileset.hpp"
#include "editor_map.hpp"
#include "gui/editor_map_component.hpp"
#include "tile.hpp"
#include "paint_command.hpp"
#include "gui/tile_selection.hpp"
#include <memory>
#include "tool_impl.hpp"

TileMapPaintTool TileMapPaintTool::current_;

class TileMapPaintToolImpl : public ToolImpl
{
public:
  enum { PAINTING, SELECTING, NONE } mode;

  TileSelection selection;
  TileBrush brush;
  CL_Point last_draw;
  CL_Point current_tile;

  std::unique_ptr<PaintCommand> command;

  void draw();

  void on_mouse_down(const CL_InputEvent& event);
  void on_mouse_move(const CL_InputEvent& event);
  void on_mouse_up  (const CL_InputEvent& event);
};

TileMapPaintTool::TileMapPaintTool()
  : impl(new TileMapPaintToolImpl())
{
  impl->last_draw = CL_Point(-1, -1);

  current_  = *this;

  impl->brush = TileBrush(1, 1);
  impl->brush.at(0, 0) = 0;
  impl->brush.set_opaque();
  impl->current_tile = CL_Point(0,0);


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
        selection.draw(CL_Color(255,  128, 128, 100));
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

            CL_Display::fill_rect(CL_Rect(CL_Point((current_tile.x + x) * tile_size,
                                                   (current_tile.y + y) * tile_size),
                                          CL_Size(tile_size, tile_size)),
                                  CL_Color(255, 255, 255, 100));
          }
          else if (brush.is_opaque())
          {
            CL_Display::fill_rect(CL_Rect(CL_Point((current_tile.x + x) * tile_size,
                                                   (current_tile.y + y) * tile_size),
                                          CL_Size(tile_size, tile_size)),
                                  CL_Color(255, 255, 255, 100));
          }
          else
          {
            CL_Display::fill_rect(CL_Rect(CL_Point((current_tile.x + x) * tile_size,
                                                   (current_tile.y + y) * tile_size),
                                          CL_Size(tile_size, tile_size)),
                                  CL_Color(255, 255, 255, 50));
          }
        }
      break;
  }
}



void
TileMapPaintToolImpl::on_mouse_down(const CL_InputEvent& event)
{
  TilemapLayer tilemap = TilemapLayer::current();

  if (!tilemap.is_null())
  {
    EditorMapComponent* parent = EditorMapComponent::current();
    if (!parent)
      return;
    CL_Point pos = tilemap.world2tile(parent->screen2world(event.mouse_pos));

    switch (mode)
    {
      case TileMapPaintToolImpl::NONE:
        switch (event.id)
        {
          case CL_MOUSE_LEFT:
            mode = TileMapPaintToolImpl::PAINTING;
            parent->capture_mouse();
            command.reset(new PaintCommand(tilemap, brush));
            command->add_point(pos);
            last_draw = pos;
            break;

          case CL_MOUSE_RIGHT:
            mode = TileMapPaintToolImpl::SELECTING;
            parent->capture_mouse();

            selection.start(tilemap, pos);
            break;
        }
        break;

      default:
        break;
    }
  }
}

void
TileMapPaintToolImpl::on_mouse_move(const CL_InputEvent& event)
{
  TilemapLayer tilemap = TilemapLayer::current();

  if (!tilemap.is_null())
  {
    EditorMapComponent* parent = EditorMapComponent::current();
    if (!parent)
      return;
    current_tile = tilemap.world2tile(parent->screen2world(event.mouse_pos));

    switch (mode)
    {
      case PAINTING:
        if (!command.get())
          break;
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
TileMapPaintToolImpl::on_mouse_up  (const CL_InputEvent& event)
{
  TilemapLayer tilemap = TilemapLayer::current();
  if (tilemap.is_null())
    return;

  EditorMapComponent* parent = EditorMapComponent::current();
  if (!parent)
    return;

  EditorMap map = parent->get_workspace().get_map();
  if (!map.is_null())
    map.modify();

  current_tile = tilemap.world2tile(parent->screen2world(event.mouse_pos));

  switch (event.id)
  {
    case CL_MOUSE_LEFT:
      if (mode == PAINTING)
      {
        parent->release_mouse();
        mode = NONE;

        if (command.get())
        {
          if (CL_Keyboard::get_keycode(CL_KEY_LSHIFT) ||
              ((current_tile.x % brush.get_width()) == (last_draw.x % brush.get_width()) &&
               (current_tile.y % brush.get_height() == (last_draw.y % brush.get_height()))))
          {
            command->add_point(current_tile);
          }

          if (!parent->get_workspace().is_null())
            parent->get_workspace().get_map().execute(command->to_command());
          command.reset();
        }

        last_draw = CL_Point(-1, -1);
      }
      break;

    case CL_MOUSE_RIGHT:
      if (mode == SELECTING)
      {
        parent->release_mouse();
        mode = NONE;

        selection.update(current_tile);
        Field<int>* field = tilemap.get_field();
        if (field)
          brush = selection.get_brush(*field);

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
  }
}

void
TileMapPaintTool::set_brush(const TileBrush& b)
{
  if (!impl.get())
    return;
  impl->brush = b;
}

const TileBrush&
TileMapPaintTool::get_brush()
{
  static TileBrush empty(1, 1);
  if (!impl.get())
    return empty;
  return impl->brush;
}

Tool
TileMapPaintTool::to_tool()
{
  if (!impl.get())
    return Tool();
  return Tool(impl);
}

/* EOF */
