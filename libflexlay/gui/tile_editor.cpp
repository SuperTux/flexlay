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

#include "tile_editor.hpp"

#include "display.hpp"
#include "math/rect.hpp"
#include "sprite.hpp"
#include "tile.hpp"

TileEditor::TileEditor(int x, int y, int w, int h) 
{
  tile = 0;
#ifdef GRUMBEL
  slots.connect(sig_paint(),      this, &TileEditor::draw);
  slots.connect(sig_mouse_move(), this, &TileEditor::mouse_move);
  slots.connect(sig_mouse_down(), this, &TileEditor::mouse_down);
  slots.connect(sig_mouse_up  (), this, &TileEditor::mouse_up);
#endif
}

TileEditor::~TileEditor()
{
}

void
TileEditor::draw()
{
#ifdef GRUMBEL
  Display::push_modelview();
  Display::add_translate(get_screen_x(), get_screen_x());

  //no_tile.draw(0, 0);
  Display::fill_rect(Rect(0, 0, 32, 32), Color(155, 0, 155));

  if (tile)
  {
    tile->get_sprite().draw(0, 0);
    Display::flush();
    for(int tile_y = 0; tile_y < 8; ++tile_y)
      for(int tile_x = 0; tile_x < 8; ++tile_x)
      {
        if (tile->get_col(tile_x, tile_y))
        {
          Display::fill_rect(Rect(tile_x*16, tile_y*16,
                                  tile_x*16 + 16, tile_y*16 + 16),
                             Color(255, 0, 0, 128));
        }
      }
    Display::flush();
    if (has_mouse_over())
    {
      Display::fill_rect(Rect(Point(int(mouse_pos.x)/16 * 16,
                                    int(mouse_pos.y)/16 * 16),
                              Size(16, 16)),
                         Color(255, 255, 255, 128));
    }
  }
  else
  {
  }

  Display::pop_modelview();
#endif
}

#ifdef GRUMBEL
void
TileEditor::mouse_move(const CL_InputEvent& event)
{
  mouse_pos = event.mouse_pos;

  if (CL_Mouse::get_keycode(CL_MOUSE_LEFT))
    paint(event.mouse_pos, true);
  else if (CL_Mouse::get_keycode(CL_MOUSE_RIGHT))
    paint(event.mouse_pos, false);
}

void
TileEditor::mouse_down(const CL_InputEvent& event)
{
  if (tile)
  {
    switch (event.id)
    {
      case CL_MOUSE_LEFT:
        paint(event.mouse_pos, true);
        break;

      case CL_MOUSE_RIGHT:
        paint(event.mouse_pos, false);
        break;

    }
  }
}

void
TileEditor::mouse_up(const CL_InputEvent& event)
{
}

#endif

void
TileEditor::paint(Point pos, bool val)
{
  if (tile)
  {
    int x = int(pos.x) / 16;
    int y = int(pos.y) / 16;

    if (x >= 0 && y >= 0
        && x < 8 && y < 8)
    {
      tile->set_col(x, y, val);
    }
  }
}

void
TileEditor::set_tile(Tile* t)
{
  tile = t;
}

/* EOF */
