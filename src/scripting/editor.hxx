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

#include <ClanLib/Signals/signal_v0.h>

#ifdef SWIGGUILE
#  include <guile/gh.h>
#endif

#include "../tilemap_layer.hxx"

#include "Python.h"

class EditorMap;
class EditorObjMap;
class EditorTileMap;
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

void          editor_map_component_set_zoom(CL_Component* c, float z);

void editor_set_brush_tile(int i);
int  editor_get_brush_tile();
void editor_set_tool(int i);
void tilemap_paint_tool_set_brush(TileBrush brush);

void tilemap_object_tool_clear_selection();

CL_Component* editor_add_tileeditor(int x, int y, int w, int h);

void tileeditor_set_tile(CL_Component* comp, int id);
void tilemap_paint_tool_set_tilemap(TilemapLayer tilemap);

int  screen_get_width();
int  screen_get_height();

void connect(CL_Signal_v0& sig, PyObject* obj);

Tileset* tileset_create(int tile_size);
#ifdef SWIGGUILE
Tileset* tileset_create_from_file(const char* resourcefile);
#endif
void tileset_set_current(Tileset* tileset);

#ifdef SWIGGUILE
// Guile Specific Stuff
SCM  editor_get_tile_selection();
void object_selector_add_brush(CL_Component* comp, const char* name, SCM brush);

void editor_objectmap_delete_objects  (EditorMapLayer* layer,SCM selection);
SCM  editor_objectmap_get_objects     (EditorMapLayer* layer);
SCM  editor_objectmap_get_object      (EditorMapLayer* layer, int id);
SCM  tilemap_object_tool_get_objects();
void tilemap_object_tool_set_objects(EditorMapLayer* layer, SCM lst);
void objectmap_tool_set_popupmenu_callback(SCM callback);
SCM get_tile_def(int id);
SCM get_tile_defs();
SCM load_xml(const char* filename);
void            editor_map_set_metadata(EditorMap* m, SCM data); 
SCM             editor_map_get_metadata(EditorMap* m); 
void            editor_tilemap_set_data(EditorMapLayer* l, SCM data);
SCM             editor_tilemap_get_data(EditorMapLayer* l);
void tileset_add_tile(Tileset* tileset, SCM data);
#endif

#endif

/* EOF */
