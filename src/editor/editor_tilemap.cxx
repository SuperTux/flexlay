//  $Id: editor_tilemap.cxx,v 1.14 2003/09/26 14:29:36 grumbel Exp $
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

#include <math.h>
#include <iostream>
#include <ClanLib/Display/display.h>
#include <ClanLib/gl.h>
#include "../windstille_level.hxx"
#include "../globals.hxx"
#include "../tile_factory.hxx"
#include "editor.hxx"
#include "editor_map.hxx"
#include "editor_tilemap.hxx"

EditorTileMap::EditorTileMap()
{
  diamond_map = 0;

  new_level(80, 30);

  brush_tile = 0;

  scrolling = false;
}

EditorTileMap::~EditorTileMap()
{
  cleanup();
}

void
EditorTileMap::new_level(int w, int h)
{
  cleanup();

  diamond_map = new Field<int>(w*2, h*2);

  current_field = new Field<EditorTile*> (w, h);
  for (int y = 0; y < current_field->get_height (); ++y) {
    for (int x = 0; x < current_field->get_width (); ++x)
      {
	current_field->at(x, y) = new EditorTile (0);
      }
  }
  fields.push_back(current_field);

  current_field = new Field<EditorTile*> (w, h);
  for (int y = 0; y < current_field->get_height (); ++y) {
    for (int x = 0; x < current_field->get_width (); ++x)
      {
	current_field->at(x, y) = new EditorTile (0);
      }
  }
  fields.push_back(current_field);
}

  
void
EditorTileMap::draw_map(Field<EditorTile*>* field)
{
  float alpha;
  if (field == current_field)
    alpha = 1.0f;
  else
    alpha = .5f;

  CL_Rect rect = Editor::current()->get_map()->get_clip_rect();

  int start_x = std::max(0, rect.left/TILE_SIZE);
  int start_y = std::max(0, rect.top/TILE_SIZE);
  int end_x   = std::min(field->get_width(),  rect.right/TILE_SIZE + 1);
  int end_y   = std::min(field->get_height(), rect.bottom/TILE_SIZE + 1);

  for (int y = start_y; y < end_y; ++y)
    {
      for (int x = start_x; x < end_x; ++x)
	{
	  field->at(x, y)->draw(x * TILE_SIZE, y * TILE_SIZE, alpha);
	}
    }
}

void
EditorTileMap::draw ()
{
  CL_Display::fill_rect(CL_Rect(CL_Point(0,0),
                                CL_Size(current_field->get_width() * TILE_SIZE,
                                        current_field->get_height() * TILE_SIZE)),
                        CL_Color(0, 0, 150, 255));
  CL_Display::flush();

  for(Fields::iterator i = fields.begin(); i != fields.end();++i) 
    draw_map(*i);
  
  CL_Display::flush();

  if (diamond_map)
    {
      for(int y = 0; y < diamond_map->get_height(); ++y)
        for(int x = 0; x < diamond_map->get_width(); ++x)
          {
            if ((*diamond_map)(x, y))
              {
                CL_Display::fill_rect(CL_Rect(CL_Point(x*64, y*64),
                                              CL_Size(64, 64)),
                                      CL_Color(255, 255, 0, 155));
              }
          }
    }
}

void
EditorTileMap::cleanup()
{
  for (Fields::iterator i = fields.begin(); i != fields.end(); ++i)
    {
      delete *i;
    }
  fields.clear();
  delete diamond_map;
  diamond_map = 0;
}

void
EditorTileMap::load(const std::string& filename)
{
  cleanup();

  WindstilleLevel data (filename);

  current_field = new Field<EditorTile*>(data.get_background_tilemap()->get_width (),
                                         data.get_background_tilemap()->get_height ());

  fields.push_back(current_field);

  for (int y = 0; y < current_field->get_height (); ++y) 
    {
      for (int x = 0; x < current_field->get_width (); ++x)
        {
          int name = data.get_background_tilemap()->at(x, y);
          current_field->at (x, y) = new EditorTile (name);
        }
    }

  current_field = new Field<EditorTile*>(data.get_tilemap()->get_width (),
                                         data.get_tilemap()->get_height ());
  fields.push_back(current_field);

  for (int y = 0; y < current_field->get_height (); ++y) {
    for (int x = 0; x < current_field->get_width (); ++x)
      {
	int name = data.get_tilemap()->at(x, y);
	current_field->at (x, y) = new EditorTile (name);
      }
  }

  diamond_map = new Field<int>(*data.get_diamond_map());
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
EditorTileMap::set_active_layer(int i)
{
  if (i >= 0 && i < int(fields.size()))
    current_field = fields[i];
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
