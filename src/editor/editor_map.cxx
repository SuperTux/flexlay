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
#include "../windstille_level.hxx"
#include "../globals.hxx"
#include "../tile_factory.hxx"
#include "tilemap_paint_tool.hxx"
#include "tilemap_select_tool.hxx"
#include "tilemap_diamond_tool.hxx"
#include "editor_map.hxx"

EditorMap::EditorMap(CL_Component* parent)
  : CL_Component(CL_Rect(CL_Point(0, 0),
                         CL_Size(CL_Display::get_width(), CL_Display::get_height())),
                 parent)
{
  slots.connect(sig_paint(),      this, &EditorMap::draw);
  slots.connect(sig_mouse_up(),   this, &EditorMap::mouse_up);
  slots.connect(sig_mouse_down(), this, &EditorMap::mouse_down);
  slots.connect(sig_mouse_move(), this, &EditorMap::mouse_move);

  diamond_map = 0;

  new_level(80, 30);

  trans_offset     = CL_Pointf(0,0);
  old_trans_offset = CL_Pointf(0,0);
  click_pos        = CL_Point(0,0);
  
  brush_tile = 0;
  zoom_factor = 0;

  scrolling = false;

  layers.push_back(tilemap = new EditorTileMap());
  layers.push_back(objmap  = new EditorObjMap());

  tools.push_back(new TileMapPaintTool(this, tilemap));
  tools.push_back(new TileMapSelectTool(this, tilemap));
  tools.push_back(new TileMapDiamondTool(this, tilemap));

  tool = tools[0];
}

EditorMap::~EditorMap()
{
  cleanup();

  for(Tools::iterator i = tools.begin(); i != tools.end(); ++i)
    delete *i;
}

void
EditorMap::new_level(int w, int h)
{
  cleanup();

  layers.push_back(tilemap = new EditorTileMap());
  layers.push_back(objmap  = new EditorObjMap());
}

void
EditorMap::mouse_up(const CL_InputEvent& event)
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
EditorMap::mouse_move(const CL_InputEvent& event)
{
  tool->on_mouse_move(event);

  if (scrolling)
    {
      trans_offset.x = old_trans_offset.x - (click_pos.x - event.mouse_pos.x);
      trans_offset.y = old_trans_offset.y - (click_pos.y - event.mouse_pos.y);
    }
}

void
EditorMap::mouse_down(const CL_InputEvent& event)
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
EditorMap::draw ()
{
  CL_Display::push_translate_offset(int(trans_offset.x), int(trans_offset.y));

  CL_Display::clear(CL_Color(100, 0, 100));

  for(Layers::iterator i = layers.begin(); i != layers.end(); ++i)
    {
      (*i)->draw();
    }

  if (1) //has_mouse_over())
    tool->draw();
    
  CL_Display::flush();

  CL_Display::pop_translate_offset();
}

CL_Point
EditorMap::screen2tile(const CL_Point& pos)
{
  int x = int(pos.x - trans_offset.x)/TILE_SIZE;
  int y = int(pos.y - trans_offset.y)/TILE_SIZE;

  return CL_Point(pos.x - trans_offset.x < 0 ? x-1 : x,
                  pos.y - trans_offset.y < 0 ? y-1 : y); 
                  
}

CL_Point
EditorMap::screen2world(const CL_Point& pos)
{
  int x = int(pos.x - trans_offset.x);
  int y = int(pos.y - trans_offset.y);

  return CL_Point(x, y);                  
}

void
EditorMap::cleanup()
{
  scripts.clear();
  for(Layers::iterator i = layers.begin(); i != layers.end(); ++i)
    {
      delete (*i);
    }
}

void
EditorMap::load(const std::string& filename)
{
  WindstilleLevel data (filename);
  new_level(10, 10);
  scripts = *data.get_scripts();
}

void
EditorMap::save (const std::string& filename)
{
}

void
EditorMap::set_tool(int i)
{
  if (i >= 0 && i < int(tools.size()))
    tool = tools[i];
  else
    {
      std::cout << "Only have " << tools.size() << " tools, tool " << i << " can't be selected." << std::endl;
    }
}

void
EditorMap::zoom_out()
{
  zoom_factor -= 1;
  std::cout << "Zoom: " << get_zoom() << std::endl;
}

void
EditorMap::zoom_in()
{
  zoom_factor += 1;
  std::cout << "Zoom: " << get_zoom() << std::endl;
}

float
EditorMap::get_zoom()
{
  if (zoom_factor > 0)
    return 1.0f * (zoom_factor + 1);
  else if (zoom_factor < 0)
    return 1.0f / (-zoom_factor + 1);
  else
    return 1.0f;
}

CL_Rect
EditorMap::get_clip_rect()
{
  return CL_Rect(CL_Point(int(0 - trans_offset.x), int(0 - trans_offset.y)),
                 CL_Size(CL_Display::get_width(), 
                         CL_Display::get_height()));
}

EditorMapLayer*
EditorMap::get_layer_by_name(int i)
{
  switch(i)
    {
    case 0:
      return tilemap;
    case 1: 
      return objmap;
    default:
      return 0;
    }
}

EditorMapLayer*
EditorMap::get_layer(int i)
{
  if (i >= 0 && i < static_cast<int>(layers.size()))
    return layers[i];
  else
    return 0;
}

int
EditorMap::get_width()
{
  assert(!"FIXME: Implement me");
  return 0;
}

int get_height()
{
  assert(!"FIXME: Implement me");
  return 0; 
}

/* EOF */
