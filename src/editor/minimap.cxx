//  $Id$
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
#include <ClanLib/Display/display.h>
#include <ClanLib/Display/pixel_format.h>
#include <ClanLib/Display/pixel_buffer.h>
#include "../globals.hxx"
#include "scripting/editor.hxx"
#include "editor.hxx"
#include "tile.hxx"
#include "tile_factory.hxx"
#include "editor_map.hxx"
#include "editor_map_component.hxx"
#include "editor_tilemap.hxx"
#include "minimap.hxx"

Minimap::Minimap(EditorMapComponent* p, const CL_Point& pos, const CL_Size& size, CL_Component* parent)
  : CL_Component(CL_Rect(pos, size), parent), 
    parent(p)
{
  slots.push_back(sig_paint().connect(this, &Minimap::draw));
  slots.push_back(sig_mouse_move().connect(this, &Minimap::mouse_move));
  slots.push_back(sig_mouse_down().connect(this, &Minimap::mouse_down));
  slots.push_back(sig_mouse_up().connect(this, &Minimap::mouse_up));

  drag_active = false;
  last_serial = -1;
  editor_map = 0;
}

void
Minimap::draw()
{
  // FIXME: Do this only on map changes
  if (last_serial != EditorMapComponent::current()->get_map()->get_serial()
      || editor_map != EditorMapComponent::current()->get_map())
    {
      update_minimap_surface();
      last_serial = EditorMapComponent::current()->get_map()->get_serial();
      editor_map  = EditorMapComponent::current()->get_map();
    }

  if (1)
    { // Draw background color
      CL_Display::fill_rect(CL_Rect(CL_Point(0, 0),
                                    CL_Size(get_width(),
                                            get_height())),
                            CL_Color(200, 200, 200, 225));
    }

  // FIXME: This doesn't work all that well
  EditorTileMap* tilemap = dynamic_cast<EditorTileMap*>
    (EditorMapComponent::current()->get_map()->get_layer(0));

  int map_width  = tilemap->get_width()  * TILE_SIZE;
  int map_height = tilemap->get_height() * TILE_SIZE;

  CL_Size small_tile(TILE_SIZE * get_width() / map_width + 1,
                     TILE_SIZE * get_height() / map_height + 1);

  Field<int>* field = tilemap->get_map();

  if (0)
    {
      for(int y = 0; y < field->get_height(); ++y)
        for(int x = 0; x < field->get_width(); ++x)
          {
            Tile* tile = TileFactory::current()->create(field->at(x, y));
            if (tile)
              CL_Display::fill_rect(CL_Rect(CL_Point((x * TILE_SIZE) * get_width() / map_width,
                                                     (y * TILE_SIZE) * get_height() / map_height),
                                            small_tile),
                                    tile->get_color());
            CL_Display::flush();
          }
    }

  minimap_surface.draw(CL_Rect(CL_Point(0, 0),
                               CL_Size(get_width(), get_height())));

  // Draw cursor
  CL_Rect rect = parent->get_clip_rect();
  CL_Rect screen_rect(CL_Point(rect.left  * get_width()  / map_width,
                               rect.top   * get_height() / map_height),
                      CL_Size(rect.get_width() * get_width() /map_width,
                              rect.get_height()* get_height()/map_height));
  CL_Display::fill_rect(screen_rect,
                        CL_Color(255, 255, 0, 50));
  CL_Display::draw_rect(screen_rect,
                        CL_Color(0, 0, 0));
}

void
Minimap::update_minimap_surface()
{
  // FIXME: This doesn't work all that well
  EditorTileMap* tilemap = EditorTileMap::current();

  Field<int>* field = tilemap->get_map();

  CL_PixelBuffer buffer(tilemap->get_width(), tilemap->get_height(), 
                        tilemap->get_width()*4, CL_PixelFormat::rgba8888);
  
  int map_width  = tilemap->get_width();
  int map_height = tilemap->get_height();

  unsigned char* buf = static_cast<unsigned char*>(buffer.get_data());
  for(int y = 0; y < map_height; ++y)
    for(int x = 0; x < map_width; ++x)
      {
        Tile* tile = TileFactory::current()->create(field->at(x, y));
        if (tile)
          {
            buf[4*(x + y * map_width) + 3] = tile->get_color().get_red();
            buf[4*(x + y * map_width) + 2] = tile->get_color().get_green();
            buf[4*(x + y * map_width) + 1] = tile->get_color().get_blue();
            buf[4*(x + y * map_width) + 0] = tile->get_color().get_alpha();
          } 
        else
          {
            buf[4*(x + y * map_width) + 0] = 0;
            buf[4*(x + y * map_width) + 1] = 0;
            buf[4*(x + y * map_width) + 2] = 0;
            buf[4*(x + y * map_width) + 3] = 0;
          }
      }

  minimap_surface = CL_Surface(&buffer);
}

void
Minimap::mouse_move(const CL_InputEvent& event)
{
  // FIXME: This doesn't work all that well
  EditorTileMap* tilemap = EditorTileMap::current();

  int map_width  = tilemap->get_width()  * TILE_SIZE;
  int map_height = tilemap->get_height() * TILE_SIZE;

  if (drag_active)
    parent->move_to(event.mouse_pos.x * map_width / get_width(),
                    event.mouse_pos.y * map_height / get_height());
}

void
Minimap::mouse_down(const CL_InputEvent& event)
{
  // FIXME: This doesn't work all that well
  EditorTileMap* tilemap = EditorTileMap::current();

  int map_width  = tilemap->get_width()  * TILE_SIZE;
  int map_height = tilemap->get_height() * TILE_SIZE;

  parent->move_to(event.mouse_pos.x * map_width / get_width(),
                  event.mouse_pos.y * map_height / get_height());
  drag_active = true;
  capture_mouse();
}

void
Minimap::mouse_up  (const CL_InputEvent& event)
{
  drag_active = false;
  release_mouse();
}

void
Minimap::update_minimap()
{
  update_minimap_surface();
}

/* EOF */
