//  $Id: scripting.hxx,v 1.6 2003/09/11 20:11:01 grumbel Exp $
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

#ifndef HEADER_SCRIPTING_HXX
#define HEADER_SCRIPTING_HXX

#include <guile/gh.h>

class CL_Component;

CL_Component* push_component(CL_Component* c);
void pop_component();

void tile_selector_create(int x, int y, int w, int h);
void editor_set_brush_tile(int i);
int  editor_get_brush_tile();
void editor_set_tool(SCM func);
void tilemap_set_active_layer(int i);

CL_Component* editor_add_window(int x, int y, int w, int h, const char* title);
CL_Component* editor_add_button_func(int x, int y, int w, int h, const char* text, SCM func);
CL_Component* editor_add_button(int x, int y, int w, int h, const char* text);
CL_Component* editor_add_label(int x, int y, const char* text);
CL_Component* editor_add_inputbox(int x, int y, int w, int h, const char* text);

void component_on_click(CL_Component* comp, SCM func);

const char* inputbox_get_text(CL_Component*);

CL_Component* window_get_client_area(CL_Component* comp);
void window_close(CL_Component* comp);
void component_hide(CL_Component* comp);
void component_show(CL_Component* comp);

void editor_quit();
int  screen_get_width();
int  screen_get_height();

SCM  map_get_data(int i);
int  map_get_width();
int  map_get_height();
void map_set_size(int w, int h);
void map_resize(int w, int h);
void map_clear();

void editor_new(int w, int h);
void editor_load(const char* filename);

void file_dialog();

#endif

/* EOF */
