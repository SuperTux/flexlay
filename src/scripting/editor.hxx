//  $Id: editor.hxx,v 1.8 2003/10/11 08:11:59 grumbel Exp $
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

#ifndef HEADER_SCRIPTING_EDITOR_HXX
#define HEADER_SCRIPTING_EDITOR_HXX

#include <guile/gh.h>

class CL_Component;
class EditorTileMap;

// FIXME: Hack
EditorTileMap* editor_get_tilemap();

CL_Component* minimap_create(CL_Component* p, int x, int y, int w, int h);
CL_Component* tile_selector_create(int x, int y, int w, int h, float scale);
CL_Component* editor_create_map(int x, int y, int w, int h);

void editor_toggle_grid();
void editor_resize_map(int w, int h, int x, int y);
void editor_tilemap_draw_brush(int pos_x, int pos_y, SCM brush);
void editor_set_brush_tile(int i);
int  editor_get_brush_tile();
void editor_set_tool(int i);
SCM  editor_get_tile_selection();
void tilemap_set_active_layer(int i);
void tilemap_paint_tool_set_brush(SCM brush);

int  editor_objectmap_add_object(const char* name, int x, int y, SCM userdata);
SCM  editor_objectmap_get_objects();
SCM  editor_objectmap_get_object(int id);

CL_Component* editor_add_tileeditor(int x, int y);

void tileeditor_set_tile(CL_Component* comp, int id);

int  screen_get_width();
int  screen_get_height();

SCM  diamond_map_get_data();
SCM  map_get_scripts();
SCM  map_get_data(int i);
int  map_get_width();
int  map_get_height();
void map_set_size(int w, int h);
void map_resize(int w, int h);
void map_clear();

void editor_new(int w, int h);
void editor_load(const char* filename);

void game_play(const char* filename);

SCM get_tile_def(int id);
SCM get_tile_defs();

#endif

/* EOF */
