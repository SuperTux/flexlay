//  Flexlay - A Generic 2D Game Editor
//  Copyright (C) 2002 Ingo Ruhnke <grumbel@gmx.de>
//
//  This program is free software: you can redistribute it and/or modify
//  it under the terms of the GNU General Public License as published by
//  the Free Software Foundation, either version 3 of the License, or
//  (at your option) any later version.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//  GNU General Public License for more details.
//
//  You should have received a copy of the GNU General Public License
//  along with this program.  If not, see <http://www.gnu.org/licenses/>.

#include "minimap.hpp"

#include <iostream>
#include <ClanLib/Display/display.h>

#include "display.hpp"
#include "editor_map.hpp"
#include "editor_map_component.hpp"
#include "lib/workspace.hpp"
#include "pixel_buffer.hpp"
#include "surface.hpp"
#include "tile.hpp"
#include "tilemap_layer.hpp"
#include "tileset.hpp"

class MinimapImpl
{
public:
  std::vector<CL_Slot> slots;
  bool drag_active;

  int last_serial;
  EditorMap editor_map;

  EditorMapComponent* parent;
  Surface minimap_surface;

  MinimapImpl()
    : editor_map(false)
  {}
  void update_minimap_surface();
};

Minimap::Minimap(EditorMapComponent* p, const Rect& rect,
                 CL_Component* parent) :
  CL_Component(rect.to_cl(), parent),
    impl(new MinimapImpl())
{
  impl->slots.push_back(sig_paint().connect(this, &Minimap::draw));
  impl->slots.push_back(sig_mouse_move().connect(this, &Minimap::mouse_move));
  impl->slots.push_back(sig_mouse_down().connect(this, &Minimap::mouse_down));
  impl->slots.push_back(sig_mouse_up().connect(this, &Minimap::mouse_up));

  impl->parent = p ? p : EditorMapComponent::current();
  impl->drag_active = false;
  impl->last_serial = -1;
}

void
Minimap::draw()
{
  if (impl->parent->get_workspace().get_map().is_null()) return;

  if (!impl->parent || impl->parent->get_workspace().is_null())
    return;

  CL_Display::push_cliprect(get_screen_rect());
  CL_Display::push_translate(get_screen_x(), get_screen_y());

  // FIXME: Do this only on map changes
  if (impl->last_serial != impl->parent->get_workspace().get_map().get_serial())
    //      || editor_map != parent->get_workspace().get_map())
  {
    impl->update_minimap_surface();
    impl->last_serial = impl->parent->get_workspace().get_map().get_serial();
    impl->editor_map  = impl->parent->get_workspace().get_map();
  }

  if (1)
  { // Draw background color
    Display::fill_rect(Rect(Point(0, 0), Size(get_width(), get_height())),
                          Color(200, 200, 200, 225));
  }

  // FIXME: This doesn't work all that well
  TilemapLayer tilemap = TilemapLayer::current();

  if (!tilemap.is_null() && tilemap.get_height() != 0 && tilemap.get_width() != 0)
  {
    int tile_size = tilemap.get_tileset().get_tile_size();

    int map_width  = tilemap.get_width()  * tile_size;
    int map_height = tilemap.get_height() * tile_size;

    Size small_tile(tile_size * get_width() / map_width + 1,
                       tile_size * get_height() / map_height + 1);

    Field<int>* field = tilemap.get_field();

    // FIXME: No current tileset
    if (0)
    {
      for(int y = 0; y < field->get_height(); ++y)
        for(int x = 0; x < field->get_width(); ++x)
        {
          Tile* tile = tilemap.get_tileset().create(field->at(x, y));
          if (tile)
            Display::fill_rect(Rect(Point((x * tile_size) * get_width() / map_width,
                                             (y * tile_size) * get_height() / map_height),
                                       Size(small_tile)),
                                  tile->get_color());
          CL_Display::flush();
        }
    }
    impl->minimap_surface.draw(Rect(Point(0, 0),
                                    Size(get_width(), get_height())));

    // Draw cursor
    Rect rect(impl->parent->get_clip_rect());
    Rect screen_rect(Point(rect.left  * get_width()  / map_width,
                           rect.top   * get_height() / map_height),
                     Size(rect.get_width() * get_width() /map_width,
                          rect.get_height()* get_height()/map_height));
    Display::fill_rect(screen_rect,
                          Color(255, 255, 0, 50));
    Display::draw_rect(screen_rect,
                          Color(0, 0, 0));
  }

  Display::pop_modelview();
  CL_Display::pop_cliprect();
}

void
MinimapImpl::update_minimap_surface()
{
  // FIXME: This doesn't work all that well
  TilemapLayer tilemap = TilemapLayer::current();

  if (!tilemap.is_null())
  {
    Field<int>* field = tilemap.get_field();

    PixelBuffer buffer(tilemap.get_width(), tilemap.get_height());

    int map_width  = tilemap.get_width();
    int map_height = tilemap.get_height();

    // FIXME: No Tileset::current()
    unsigned char* buf = static_cast<unsigned char*>(buffer.get_data());
    for(int y = 0; y < map_height; ++y)
      for(int x = 0; x < map_width; ++x)
      {
        Tile* tile = tilemap.get_tileset().create(field->at(x, y));
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

    minimap_surface = Surface(buffer);
  }
}

void
Minimap::mouse_move(const CL_InputEvent& event)
{
  // FIXME: This doesn't work all that well
  TilemapLayer tilemap = TilemapLayer::current();
  if (!tilemap.is_null())
  {
    int tile_size  = tilemap.get_tileset().get_tile_size();
    int map_width  = tilemap.get_width()  * tile_size;
    int map_height = tilemap.get_height() * tile_size;

    if (impl->drag_active)
      impl->parent->move_to(event.mouse_pos.x * map_width / get_width(),
                            event.mouse_pos.y * map_height / get_height());
  }
}

void
Minimap::mouse_down(const CL_InputEvent& event)
{
  // FIXME: This doesn't work all that well
  TilemapLayer tilemap = TilemapLayer::current();
  if (!tilemap.is_null())
  {
    int tile_size  = tilemap.get_tileset().get_tile_size();
    int map_width  = tilemap.get_width()  * tile_size;
    int map_height = tilemap.get_height() * tile_size;

    impl->parent->move_to(event.mouse_pos.x * map_width / get_width(),
                          event.mouse_pos.y * map_height / get_height());
    impl->drag_active = true;
    capture_mouse();
  }
}

void
Minimap::mouse_up  (const CL_InputEvent& event)
{
  TilemapLayer tilemap = TilemapLayer::current();
  if (!tilemap.is_null())
  {
    impl->drag_active = false;
    release_mouse();
  }
}

void
Minimap::update_minimap()
{
  impl->update_minimap_surface();
}

/* EOF */
