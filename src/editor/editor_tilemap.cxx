//  $Id: editor_tilemap.cxx,v 1.5 2003/09/10 13:53:11 grumbel Exp $
//
//  Pingus - A free Lemmings clone
//  Copyright (C) 2000 Ingo Ruhnke <grumbel@gmx.de>
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
#include "../windstille_level.hxx"
#include "../globals.hxx"
#include "../tile_factory.hxx"
#include "editor_tilemap.hxx"

EditorTileMap::EditorTileMap(CL_Component* parent)
  : CL_Component(CL_Rect(CL_Point(0, 0), CL_Size(CL_Display::get_width(), CL_Display::get_height())),
                 parent),
    field (new Field<EditorTile*> (50, 50))
{
  for (unsigned int y = 0; y < field->get_height (); ++y) {
    for (unsigned int x = 0; x < field->get_width (); ++x)
      {
	field->at(x, y) = new EditorTile (0);
      }
  }

  slots.connect(sig_paint(), this, &EditorTileMap::draw);
  slots.connect(sig_mouse_up(),   this, &EditorTileMap::mouse_up);
  slots.connect(sig_mouse_down(), this, &EditorTileMap::mouse_down);
  slots.connect(sig_mouse_move(), this, &EditorTileMap::mouse_move);

  trans_offset = CL_Pointf(0,0);
  old_trans_offset = CL_Pointf(0,0);
  click_pos = CL_Point(0,0);
  
  tool = NONE;
  brush_tile = 0;
}

void
EditorTileMap::mouse_up(const CL_InputEvent& event)
{
  switch (tool)
    {
    case SCROLLING:
      if (event.id == CL_MOUSE_MIDDLE)
        {
          trans_offset.x = old_trans_offset.x - (click_pos.x - event.mouse_pos.x);
          trans_offset.y = old_trans_offset.y - (click_pos.y - event.mouse_pos.y);
          
          old_trans_offset = trans_offset;
          tool = NONE;
          release_mouse();
        }
      break;
    case PAINTING:
      if (event.id == CL_MOUSE_LEFT)
        tool = NONE;
      break;
    }
}

void
EditorTileMap::mouse_move(const CL_InputEvent& event)
{
  switch (tool)
    {
    case SCROLLING:
      trans_offset.x = old_trans_offset.x - (click_pos.x - event.mouse_pos.x);
      trans_offset.y = old_trans_offset.y - (click_pos.y - event.mouse_pos.y);
      break;

    case PAINTING:
      {
        CL_Point pos = screen2tile(event.mouse_pos);
        field->at(pos.x, pos.y)->set_tile(brush_tile);
      }
      break;
      
    default:
      break;
    }
}

void
EditorTileMap::mouse_down(const CL_InputEvent& event)
{
  if (event.id == CL_MOUSE_MIDDLE)
    {
      old_trans_offset = trans_offset;
      click_pos = event.mouse_pos;
      tool = SCROLLING;
      capture_mouse();
    }
  else if (event.id == CL_MOUSE_LEFT)
    {
      std::cout << "brushtile: " << brush_tile << std::endl;
      CL_Point pos = screen2tile(event.mouse_pos);
      field->at(pos.x, pos.y)->set_tile(brush_tile);
      tool = PAINTING;
    }
}
  
void
EditorTileMap::draw ()
{
  CL_Display::clear(CL_Color::black);
  CL_Display::push_translate_offset(int(trans_offset.x), int(trans_offset.y));
  for (unsigned int y = 0; y < field->get_height (); ++y)
    {
      for (unsigned int x = 0; x < field->get_width (); ++x)
	{
	  field->at(x, y)->draw(x * TILE_SIZE, y * TILE_SIZE);
	}
    }

  if (1)
    {
    CL_Point pos =  screen2tile(CL_Point(CL_Mouse::get_x(), CL_Mouse::get_y()));

    if (pos.x >= 0 && pos.y >= 0)
      {
        Tile* tile = TileFactory::current()->create(brush_tile);
        if (tile)
          {
            CL_Sprite sprite = tile->sur;
            sprite.set_alpha(0.5f);

            sprite.draw(pos.x * TILE_SIZE, pos.y * TILE_SIZE);
          }
        CL_Display::fill_rect (CL_Rect(CL_Point(pos.x * TILE_SIZE, pos.y * TILE_SIZE),
                                       CL_Size(TILE_SIZE, TILE_SIZE)),
                               CL_Color(255, 255, 255, 100));
      }
  }

  CL_Display::pop_translate_offset();
}

CL_Point
EditorTileMap::screen2tile(const CL_Point& pos)
{
  return CL_Point(int(pos.x - trans_offset.x)/TILE_SIZE,
                  int(pos.y - trans_offset.y)/TILE_SIZE);
}

void
EditorTileMap::load(const std::string& filename)
{
  if (field)
    delete field;

  WindstilleLevel data (filename);

  field = new Field<EditorTile*> (data.get_tilemap()->get_width (),
				  data.get_tilemap()->get_height ());

  for (unsigned int y = 0; y < field->get_height (); ++y) {
    for (unsigned int x = 0; x < field->get_width (); ++x)
      {
	int name = data.get_tilemap()->at(x, y);
	field->at (x, y) = new EditorTile (name);
      }
  }
}

EditorTile*
EditorTileMap::get_tile (int x, int y)
{
  if (x >= 0 && x < (int) field->get_width () &&
      y >= 0 && y < (int) field->get_height ())
    return field->at (x, y);
  else
    return 0;
}

void
EditorTileMap::save (const std::string& filename)
{
#if 0
  std::ofstream out (filename.c_str ());
  
  if (out)
    {
      out << "<windstille-level>\n"
	  << "  <properties>\n"
	  << "    <levelname>Bla</levelname>\n"
	  << "  </properties>\n" << std::endl;

      out << "<tilemap width=\"" << field->get_width () << "\" height=\"" << field->get_height () << "\">";

      for (unsigned int y = 0; y < field->get_height (); ++y)
	{
	  out << "<row>" << std::endl;
	  for (unsigned int x = 0; x < field->get_width (); ++x)
	    {
	      out << "<tile>" << field->at (x, y)->get_id() << "</tile>" << std::endl;
	    }
	  out << "</row>" << std::endl;
	}

      out << "</tilemap>";
      out << "</windstille-level>" << std::endl;
    }
  else
    {
      std::cout << "Write error" << std::endl;
    }
#endif
}

/* EOF */
