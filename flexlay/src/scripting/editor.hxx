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

#include <string>
#include <vector>
#include <guile/gh.h>

class EditorMap;
class EditorObjMap;
class EditorTileMap;
class EditorMapLayer;
class CL_Component;
class TileBrush;
class Tileset;

/** Add a ClanLib resource file to the resource manager */
void game_load_resources(const char* resourcefile);

void editor_redo();
void editor_undo();

void set_window_title(const char* name);

CL_Component* minimap_create(CL_Component* p, int x, int y, int w, int h);

CL_Component* tile_selector_create(int x, int y, int w, int h, float scale);
void tile_selector_set_tiles(CL_Component*, const std::vector<int>& tiles);
void tile_selector_set_tileset(CL_Component*, Tileset* tileset);

CL_Component* object_selector_create(int x, int y, int w, int h, int obj_w, int obj_h);
CL_Component* editor_map_component_create(int x, int y, int w, int h);
void          editor_map_component_set_map(CL_Component* c, EditorMap* m);
EditorMap*    editor_map_component_get_map(CL_Component* c);
void          editor_map_component_set_zoom(CL_Component* c, float z);

void editor_set_brush_tile(int i);
int  editor_get_brush_tile();
void editor_set_tool(int i);
SCM  editor_get_tile_selection();
void tilemap_paint_tool_set_brush(TileBrush brush);
void tilemap_paint_tool_set_tilemap(EditorMapLayer* layer);

void object_selector_add_brush(CL_Component* comp, const char* name, SCM brush);

int  objectmap_add_object(EditorMapLayer* obj, const char* name, int x, int y, SCM userdata);

int  editor_objectmap_add_object      (EditorMapLayer* layer, const char* name, int x, int y, SCM userdata);
void editor_objectmap_delete_objects  (EditorMapLayer* layer,SCM selection);
int  editor_objectmap_duplicate_object(EditorMapLayer* layer, int id);
SCM  editor_objectmap_get_objects     (EditorMapLayer* layer);
SCM  editor_objectmap_get_object      (EditorMapLayer* layer, int id);
void editor_objectmap_set_pos         (EditorMapLayer* layer, int id, int x, int y);

void editor_tilemap_set_current(EditorMapLayer* layer);
void editor_objectmap_set_current(EditorMapLayer* layer);

void objmap_sprite_object_flip(EditorMapLayer* layer, int id);
SCM  tilemap_object_tool_get_objects();
void tilemap_object_tool_set_objects(EditorMapLayer* layer, SCM lst);
void tilemap_object_tool_clear_selection();

void objectmap_tool_set_popupmenu_callback(SCM callback);

CL_Component* editor_add_tileeditor(int x, int y, int w, int h);

void tileeditor_set_tile(CL_Component* comp, int id);

int  screen_get_width();
int  screen_get_height();

SCM get_tile_def(int id);
SCM get_tile_defs();
SCM load_xml(const char* filename);

// Map stuff
EditorMap*      editor_map_create();
void            editor_map_set_metadata(EditorMap* m, SCM data); 
SCM             editor_map_get_metadata(EditorMap* m); 
void            editor_map_add_layer(EditorMap* m, EditorMapLayer* layer);
bool            editor_map_is_modified(EditorMap* m);
void            editor_map_set_unmodified(EditorMap* m);

EditorMapLayer* editor_grid_layer_create(int w, int h, int tile_size);
EditorMapLayer* editor_objmap_create();

std::string     editor_map_get_filename(EditorMap* m);
void            editor_map_set_filename(EditorMap* m, const char* name);

void            editor_toggle_grid(EditorMapLayer* layer);
void            editor_toggle_attributes(EditorMapLayer* layer);
EditorMapLayer* editor_tilemap_create(Tileset* tileset, int w, int h, int tile_size);
void            editor_tilemap_resize(EditorMapLayer* , int w, int h, int x, int y);
void            editor_tilemap_set_data(EditorMapLayer* l, SCM data);
SCM             editor_tilemap_get_data(EditorMapLayer* l);
int             editor_tilemap_get_width(EditorMapLayer* l);
int             editor_tilemap_get_height(EditorMapLayer* l);
void            editor_tilemap_set_bgcolor(EditorMapLayer* l, int r, int g, int b, int a);
void            editor_tilemap_set_fgcolor(EditorMapLayer* l, int r, int g, int b, int a);
void            editor_tilemap_save_png(EditorMapLayer* l, const char* filename);

Tileset* tileset_create(int tile_size);
Tileset* tileset_create_from_file(const char* resourcefile);
void tileset_add_tile(Tileset* tileset, SCM data);
void tileset_set_current(Tileset* tileset);

#endif

/* EOF */
