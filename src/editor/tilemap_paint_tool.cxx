//  $Id: tilemap_paint_tool.cxx,v 1.2 2003/09/23 22:07:32 grumbel Exp $
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

#include <iostream>
#include <ClanLib/Display/mouse.h>
#include <ClanLib/Display/keyboard.h>
#include <ClanLib/Display/keys.h>
#include <ClanLib/Display/display.h>
#include "globals.hxx"
#include "editor_tilemap.hxx"
#include "tile_factory.hxx"
#include "editor_map.hxx"
#include "editor_map_component.hxx"
#include "../tile.hxx"
#include "editor.hxx"
#include "paint_command.hxx"
#include "tilemap_paint_tool.hxx"

TileMapPaintTool* TileMapPaintTool::current_ = 0; 

TileMapPaintTool::TileMapPaintTool()
{
  last_draw = CL_Point(-1, -1);

  current_  = this;
  brush = TileBrush(1, 1);
  brush.at(0, 0) = 0;
  brush.set_opaque();
  current_tile = CL_Point(0,0);

  command = 0;

  mode = NONE;
}

TileMapPaintTool::~TileMapPaintTool()
{
}

void
TileMapPaintTool::draw()
{
  switch(mode)
    {
    case SELECTING:
      if (CL_Keyboard::get_keycode(CL_KEY_LSHIFT))
        selection.draw(CL_Color(255,  128, 128, 100));
      else 
        selection.draw();
      break;
      
    default:
      // Draw the brush:
      for(int y = 0; y < brush.get_height(); ++y)
        for(int x = 0; x < brush.get_width(); ++x)
          {
            Tile* tile = TileFactory::current()->create(brush(x, y));
                
            if (tile)
              {
                CL_Sprite sprite = tile->sur;
                sprite.set_alpha(0.5f);
                sprite.draw((current_tile.x + x) * TILE_SIZE, 
                            (current_tile.y + y) * TILE_SIZE);

                CL_Display::fill_rect(CL_Rect(CL_Point((current_tile.x + x) * TILE_SIZE, 
                                                       (current_tile.y + y) * TILE_SIZE),
                                              CL_Size(TILE_SIZE, TILE_SIZE)),
                                      CL_Color(255, 255, 255, 100));
              }
            else if (brush.is_opaque())
              {
                CL_Display::fill_rect(CL_Rect(CL_Point((current_tile.x + x) * TILE_SIZE, 
                                                       (current_tile.y + y) * TILE_SIZE),
                                              CL_Size(TILE_SIZE, TILE_SIZE)),
                                      CL_Color(255, 255, 255, 100));
              }
            else
              {
                CL_Display::fill_rect(CL_Rect(CL_Point((current_tile.x + x) * TILE_SIZE, 
                                                       (current_tile.y + y) * TILE_SIZE),
                                              CL_Size(TILE_SIZE, TILE_SIZE)),
                                      CL_Color(255, 255, 255, 50));
              }
          }
      break;
    }
}

void
TileMapPaintTool::on_mouse_down(const CL_InputEvent& event)
{
  EditorTileMap* tilemap = EditorTileMap::current();
  EditorMapComponent* parent = EditorMapComponent::current();
  CL_Point pos = parent->screen2tile(event.mouse_pos);

  switch (mode)
    {
    case NONE:
      switch (event.id)
        {
        case CL_MOUSE_LEFT:
          mode = PAINTING;
          parent->capture_mouse();
          command = new PaintCommand(tilemap->get_field(), brush);
          command->add_point(pos);

          tilemap->draw_tile(brush, pos);
          last_draw = pos;
          break;
    
        case CL_MOUSE_RIGHT:
          mode = SELECTING;
          parent->capture_mouse();

          selection.start(pos);
          break;
        }
      break;

    default:
      break;
    }
}
 
void
TileMapPaintTool::on_mouse_move(const CL_InputEvent& event)
{
  EditorTileMap* tilemap = EditorTileMap::current();
  EditorMapComponent* parent = EditorMapComponent::current();
  current_tile = parent->screen2tile(event.mouse_pos);

  switch (mode)
    {
    case PAINTING:
      if (current_tile != last_draw)
        {
          command->add_point(current_tile);
          tilemap->draw_tile(brush, current_tile);
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

void
TileMapPaintTool::on_mouse_up  (const CL_InputEvent& event)
{
  EditorTileMap* tilemap = EditorTileMap::current();
  EditorMapComponent* parent = EditorMapComponent::current();
  CL_Point pos = parent->screen2tile(event.mouse_pos);

  if (mode == PAINTING || mode == SELECTING)
    {
      switch (event.id)
        {
        case CL_MOUSE_LEFT:
          parent->release_mouse();
          mode = NONE;

          command->add_point(pos);
          Editor::current()->execute(command);
          command = 0;

          tilemap->draw_tile(brush, pos);
          last_draw = CL_Point(-1, -1);
          break;
    
        case CL_MOUSE_RIGHT:
          parent->release_mouse();
          mode = NONE;

          selection.update(pos);
          brush = selection.get_brush(*tilemap->get_field());

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
          break;
        }
    }
}

void
TileMapPaintTool::set_brush(const TileBrush& b)
{
  brush = b;
}

/* EOF */
