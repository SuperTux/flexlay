//  $Id: tile_editor.cxx,v 1.1 2003/09/22 18:37:05 grumbel Exp $
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

#include <iostream>
#include <ClanLib/gui.h>
#include <ClanLib/Display/display.h>
#include <ClanLib/Display/keys.h>
#include <ClanLib/Display/mouse.h>
#include "tile.hxx"
#include "globals.hxx"
#include "tile_editor.hxx"

TileEditor::TileEditor(int x, int y, int w, int h, CL_Component* parent)
  : CL_Component(CL_Rect(CL_Rect(CL_Point(x, y), 
                                 CL_Size(w, h))), // FIXME: make this editable via script
                 parent)
{
  tile = 0;
  slots.connect(sig_paint(),      this, &TileEditor::draw);
  slots.connect(sig_mouse_move(), this, &TileEditor::mouse_move);
  slots.connect(sig_mouse_down(), this, &TileEditor::mouse_down);
  slots.connect(sig_mouse_up  (), this, &TileEditor::mouse_up);
}

TileEditor::~TileEditor()
{
}
  
void
TileEditor::draw()
{
  CL_Display::push_translate(get_screen_x(), get_screen_x());

  //no_tile.draw(0, 0);
  CL_Display::fill_rect(CL_Rect(0, 0, 32, 32), CL_Color(155, 0, 155));

  if (tile)
    {
      tile->get_sprite().draw(0, 0);
      CL_Display::flush();
      for(int tile_y = 0; tile_y < 8; ++tile_y)
        for(int tile_x = 0; tile_x < 8; ++tile_x)
          {
            if (tile->get_col(tile_x, tile_y))
              {
                CL_Display::fill_rect(CL_Rect(tile_x*16, tile_y*16,
                                              tile_x*16 + 16, tile_y*16 + 16),
                                      CL_Color(255, 0, 0, 128));
              }
          }
      CL_Display::flush();
      if (has_mouse_over())
        {
          CL_Display::fill_rect(CL_Rect(CL_Point(int(mouse_pos.x)/16 * 16, 
                                                 int(mouse_pos.y)/16 * 16),
                                        CL_Size(16, 16)),
                                CL_Color(255, 255, 255, 128));
        }
    }
  else
    {
    }

  CL_Display::pop_modelview();
}

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
TileEditor::paint(CL_Point pos, bool val)
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
TileEditor::mouse_up(const CL_InputEvent& event)
{
}

void
TileEditor::set_tile(Tile* t)
{
  tile = t;
}

/* EOF */
