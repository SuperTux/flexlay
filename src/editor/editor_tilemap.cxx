//  $Id: editor_tilemap.cxx,v 1.7 2003/09/11 18:58:19 grumbel Exp $
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
#include <ClanLib/gl.h>
#include "../windstille_level.hxx"
#include "../globals.hxx"
#include "../tile_factory.hxx"
#include "editor_tilemap.hxx"

EditorTileMap::EditorTileMap(CL_Component* parent)
  : CL_Component(CL_Rect(CL_Point(0, 0), CL_Size(CL_Display::get_width(), CL_Display::get_height())),
                 parent)
{
  slots.connect(sig_paint(), this, &EditorTileMap::draw);
  slots.connect(sig_mouse_up(),   this, &EditorTileMap::mouse_up);
  slots.connect(sig_mouse_down(), this, &EditorTileMap::mouse_down);
  slots.connect(sig_mouse_move(), this, &EditorTileMap::mouse_move);

  new_level(80, 30);

  trans_offset = CL_Pointf(0,0);
  old_trans_offset = CL_Pointf(0,0);
  click_pos = CL_Point(0,0);
  
  tool = NONE;
  brush_tile = 0;
  zoom = 1.0f;
}

EditorTileMap::~EditorTileMap()
{
  cleanup();
}

void
EditorTileMap::new_level(int w, int h)
{
  cleanup();

  current_field = new Field<EditorTile*> (w, h);
  for (unsigned int y = 0; y < current_field->get_height (); ++y) {
    for (unsigned int x = 0; x < current_field->get_width (); ++x)
      {
	current_field->at(x, y) = new EditorTile (0);
      }
  }
  fields.push_back(current_field);

  current_field = new Field<EditorTile*> (w, h);
  for (unsigned int y = 0; y < current_field->get_height (); ++y) {
    for (unsigned int x = 0; x < current_field->get_width (); ++x)
      {
	current_field->at(x, y) = new EditorTile (0);
      }
  }
  fields.push_back(current_field);
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
    case NONE:
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
        current_field->at(pos.x, pos.y)->set_tile(brush_tile);
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
      current_field->at(pos.x, pos.y)->set_tile(brush_tile);
      tool = PAINTING;
    }
  else if (event.id == CL_MOUSE_WHEEL_UP)
    {
      zoom_in();
    }
  else if (event.id == CL_MOUSE_WHEEL_DOWN)
    {
      zoom_out();
    }
}
  
void
EditorTileMap::draw_map(Field<EditorTile*>* field)
{
  float alpha;
  if (field == current_field)
    alpha = 1.0f;
  else
    alpha = .5f;

  for (unsigned int y = 0; y < field->get_height (); ++y)
    {
      for (unsigned int x = 0; x < field->get_width (); ++x)
	{
	  field->at(x, y)->draw(x * TILE_SIZE, y * TILE_SIZE, alpha);
	}
    }

  if (has_mouse_over())
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
}

void
EditorTileMap::draw ()
{
  glPushMatrix();
  glScalef(zoom, zoom, 1.0f);
  CL_Display::push_translate_offset(int(trans_offset.x), int(trans_offset.y));

  CL_Display::clear(CL_Color(100, 0, 100));

  CL_Display::fill_rect(CL_Rect(CL_Point(0,0),
                                CL_Size(current_field->get_width() * TILE_SIZE,
                                        current_field->get_height() * TILE_SIZE)),
                        CL_Color::black);
  for(Fields::iterator i = fields.begin(); i != fields.end();++i) 
    {
      draw_map(*i);
    }

  CL_Display::pop_translate_offset();
  glPopMatrix();
}

CL_Point
EditorTileMap::screen2tile(const CL_Point& pos)
{
  return CL_Point(int(pos.x - trans_offset.x)/TILE_SIZE,
                  int(pos.y - trans_offset.y)/TILE_SIZE);
}

void
EditorTileMap::cleanup()
{
  for (Fields::iterator i = fields.begin(); i != fields.end(); ++i)
    {
      delete *i;
    }
  fields.clear();
}

void
EditorTileMap::load(const std::string& filename)
{
  cleanup();

  WindstilleLevel data (filename);

  current_field = new Field<EditorTile*>(data.get_background_tilemap()->get_width (),
                                         data.get_background_tilemap()->get_height ());

  fields.push_back(current_field);

  for (unsigned int y = 0; y < current_field->get_height (); ++y) {
    for (unsigned int x = 0; x < current_field->get_width (); ++x)
      {
	int name = data.get_background_tilemap()->at(x, y);
	current_field->at (x, y) = new EditorTile (name);
      }
  }

  current_field = new Field<EditorTile*>(data.get_tilemap()->get_width (),
                                         data.get_tilemap()->get_height ());
  fields.push_back(current_field);

  for (unsigned int y = 0; y < current_field->get_height (); ++y) {
    for (unsigned int x = 0; x < current_field->get_width (); ++x)
      {
	int name = data.get_tilemap()->at(x, y);
	current_field->at (x, y) = new EditorTile (name);
      }
  }

}

EditorTile*
EditorTileMap::get_tile (int x, int y)
{
  if (x >= 0 && x < (int)current_field->get_width () &&
      y >= 0 && y < (int)current_field->get_height ())
    return current_field->at (x, y);
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

void
EditorTileMap::set_active_layer(int i)
{
  if (i >= 0 && i < int(fields.size()))
    current_field = fields[i];
}

void
EditorTileMap::set_tool(int i)
{
  
}

void
EditorTileMap::zoom_out()
{
  std::cout << "zoomout" << std::endl;
}

void
EditorTileMap::zoom_in()
{
  std::cout << "zoomin" << std::endl;
}

Field<EditorTile*>* 
EditorTileMap::get_map(int i)
{
  if (i >= 0 && i < int(fields.size()))
    return fields[i];
  else
    return 0;
}

/* EOF */
