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
#include "tilemap_paint_tool.hxx"
#include "tilemap_select_tool.hxx"
#include "tilemap_diamond_tool.hxx"
#include "editor_tilemap.hxx"

EditorTileMap::EditorTileMap(CL_Component* parent)
  : CL_Component(CL_Rect(CL_Point(0, 0), CL_Size(CL_Display::get_width(), CL_Display::get_height())),
                 parent)
{
  slots.connect(sig_paint(), this, &EditorTileMap::draw);
  slots.connect(sig_mouse_up(),   this, &EditorTileMap::mouse_up);
  slots.connect(sig_mouse_down(), this, &EditorTileMap::mouse_down);
  slots.connect(sig_mouse_move(), this, &EditorTileMap::mouse_move);

  diamond_map = 0;

  new_level(80, 30);

  trans_offset     = CL_Pointf(0,0);
  old_trans_offset = CL_Pointf(0,0);
  click_pos        = CL_Point(0,0);
  
  brush_tile = 0;
  zoom_factor = 0;

  scrolling = false;

  tools.push_back(new TileMapPaintTool(this));
  tools.push_back(new TileMapSelectTool(this));
  tools.push_back(new TileMapDiamondTool(this));

  tool = tools[0];
}

EditorTileMap::~EditorTileMap()
{
  cleanup();

  for(Tools::iterator i = tools.begin(); i != tools.end(); ++i)
    delete *i;
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
EditorTileMap::mouse_up(const CL_InputEvent& event)
{
  switch (event.id)
    {
    case CL_MOUSE_LEFT:
    case CL_MOUSE_RIGHT:
      tool->on_mouse_up(event);
      break;

    case CL_MOUSE_MIDDLE:
      scrolling = false;
      trans_offset.x = old_trans_offset.x - (click_pos.x - event.mouse_pos.x);
      trans_offset.y = old_trans_offset.y - (click_pos.y - event.mouse_pos.y);
          
      old_trans_offset = trans_offset;
      release_mouse();
      break;
    }
}

void
EditorTileMap::mouse_move(const CL_InputEvent& event)
{
  tool->on_mouse_move(event);

  if (scrolling)
    {
      trans_offset.x = old_trans_offset.x - (click_pos.x - event.mouse_pos.x);
      trans_offset.y = old_trans_offset.y - (click_pos.y - event.mouse_pos.y);
    }
}

void
EditorTileMap::mouse_down(const CL_InputEvent& event)
{
  switch (event.id)
    {
    case CL_MOUSE_LEFT:
    case CL_MOUSE_RIGHT:
      tool->on_mouse_down(event);
      break;

    case CL_MOUSE_MIDDLE:
      scrolling = true;
      old_trans_offset = trans_offset;
      click_pos = event.mouse_pos;
      capture_mouse();
      break;
           
    case CL_MOUSE_WHEEL_UP:
      zoom_in();
      break;

    case CL_MOUSE_WHEEL_DOWN:
      zoom_out();
      break;
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

  CL_Rect rect = get_clip_rect();

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
  //CL_Display::get_current_window()->get_gc()->flush();
  //glPushMatrix();

  //glScalef(get_zoom(), get_zoom(), 1.0f);
  //glTranslatef(trans_offset.x, trans_offset.y, 0.0f);

  CL_Display::push_translate_offset(int(trans_offset.x), int(trans_offset.y));

  CL_Display::clear(CL_Color(100, 0, 100));

  CL_Display::fill_rect(CL_Rect(CL_Point(0,0),
                                CL_Size(current_field->get_width() * TILE_SIZE,
                                        current_field->get_height() * TILE_SIZE)),
                        CL_Color(0, 0, 150, 255));

  CL_Display::flush();

  for(Fields::iterator i = fields.begin(); i != fields.end();++i) 
    draw_map(*i);
  
  // Draw 'cursor'
  if (1) //has_mouse_over())
    {
      tool->draw();
    }
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

  CL_Display::pop_translate_offset();
  // glPopMatrix();
}

CL_Point
EditorTileMap::screen2tile(const CL_Point& pos)
{
  int x = int(pos.x - trans_offset.x)/TILE_SIZE;
  int y = int(pos.y - trans_offset.y)/TILE_SIZE;

  return CL_Point(pos.x - trans_offset.x < 0 ? x-1 : x,
                  pos.y - trans_offset.y < 0 ? y-1 : y); 
                  
}

CL_Point
EditorTileMap::screen2world(const CL_Point& pos)
{
  int x = int(pos.x - trans_offset.x);
  int y = int(pos.y - trans_offset.y);

  return CL_Point(x, y);                  
}

void
EditorTileMap::cleanup()
{
  scripts.clear();
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
  scripts = *data.get_scripts();
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
  if (i >= 0 && i < int(tools.size()))
    tool = tools[i];
  else
    {
      std::cout << "Only have " << tools.size() << " tools, tool " << i << " can't be selected." << std::endl;
    }
}

void
EditorTileMap::zoom_out()
{
  zoom_factor -= 1;
  std::cout << "Zoom: " << get_zoom() << std::endl;
}

void
EditorTileMap::zoom_in()
{
  zoom_factor += 1;
  std::cout << "Zoom: " << get_zoom() << std::endl;
}

float
EditorTileMap::get_zoom()
{
  if (zoom_factor > 0)
    return 1.0f * (zoom_factor + 1);
  else if (zoom_factor < 0)
    return 1.0f / (-zoom_factor + 1);
  else
    return 1.0f;
}

Field<EditorTile*>* 
EditorTileMap::get_map(int i)
{
  if (i >= 0 && i < int(fields.size()))
    return fields[i];
  else
    return 0;
}

CL_Rect
EditorTileMap::get_clip_rect()
{
  return CL_Rect(CL_Point(int(0 - trans_offset.x), int(0 - trans_offset.y)),
                 CL_Size(CL_Display::get_width(), 
                         CL_Display::get_height()));
}

/* EOF */
