//  $Id: editor.cxx,v 1.7 2003/10/11 08:11:59 grumbel Exp $
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

#include <ClanLib/gui.h>
#include <ClanLib/Display/display.h>
#include <iostream>
#include "../scm_functor.hxx"
#include "../globals.hxx"
#include "../windstille_game.hxx"
#include "../scm_obj.hxx"
#include "editor/editor.hxx"
#include "editor/tile_selector.hxx"
#include "editor/editor_tilemap.hxx"
#include "editor/object_selector.hxx"
#include "editor/editor_map.hxx"
#include "editor/tile_editor.hxx"
#include "editor/tilemap_select_tool.hxx"
#include "editor/tilemap_paint_tool.hxx"
#include "editor/minimap.hxx"
#include "editor/editor_names.hxx"
#include "tile_factory.hxx"
#include "tile.hxx"
#include "gui_manager.hxx"
#include "editor.hxx"

extern CL_ResourceManager* resources;

EditorObjMap*
editor_get_objmap()
{
  return dynamic_cast<EditorObjMap*>(EditorMap::current()->get_layer_by_name(OBJECTMAP_NAME));
}

EditorTileMap*
editor_get_tilemap()
{
  if (EditorTileMap::current())
    return EditorTileMap::current();
  else
    {
      assert(!"Error: Tilemap not found");
      return 0;
    }
}

int editor_objectmap_add_object(const char* name, int x, int y, SCM userdata)
{
  int handle = editor_get_objmap()->add_object(CL_Sprite(name, resources), CL_Point(x, y), 
                                               SCMObj(userdata));
  return handle;
}

void
editor_set_brush_tile(int i)
{
  //editor_get_tilemap()->brush_tile = i;

  TileBrush brush(1, 1);

  brush.set_opaque();
  brush(0, 0) = i;

  TileMapPaintTool::current()->set_brush(brush);
}

void
editor_toggle_grid()
{
  editor_get_tilemap()->set_draw_grid(!editor_get_tilemap()->get_draw_grid());
}

SCM
obj2scm(const EditorObjMap::Obj& obj)
{
  SCM lst = SCM_EOL;
  
  return gh_list(gh_long2scm(obj.pos.x),
                 gh_long2scm(obj.pos.y),
                 obj.data.get_scm(),
                 SCM_UNDEFINED); 
  return lst;
}

void
object_selector_add_brush(CL_Component* comp, const char* name, SCM data)
{
  ObjectSelector* selector = dynamic_cast<ObjectSelector*>(comp);
  selector->add_brush(ObjectBrush(CL_Sprite(name, resources), SCMObj(data)));
}

SCM
brush2scm(const TileBrush& brush)
{
  std::cout << "Brush: " << brush.get_width() << "x" << brush.get_height() << std::endl;
  SCM vec = scm_c_make_vector(brush.get_width() * brush.get_height(),
                              gh_long2scm(0));

  for(int y = 0; y < brush.get_height(); ++y)
    for(int x = 0; x < brush.get_width(); ++x)
      {
        scm_vector_set_x(vec, 
                         SCM_MAKINUM(x + y * brush.get_width()),
                         SCM_MAKINUM(brush.at(x, y)));
      }
  
  return gh_list(gh_long2scm(brush.get_width()),
                 gh_long2scm(brush.get_height()),
                 gh_bool2scm(brush.is_opaque()),
                 vec,
                 SCM_UNDEFINED);
}

TileBrush
scm2brush(SCM s_brush)
{
  int brush_width  = gh_scm2int(gh_car(s_brush));
  int brush_height = gh_scm2int(gh_cadr(s_brush));
  bool opaque      = gh_scm2bool(gh_caddr(s_brush));
  SCM brush_data   = gh_car(gh_cdddr(s_brush));

  assert(brush_width*brush_height == static_cast<int>(gh_vector_length(brush_data)));

  TileBrush brush(brush_width, brush_height);

  if (opaque) 
    brush.set_opaque();
  else
    brush.set_transparent();

  for(int i = 0; i < brush.size(); ++i)
    {
      brush[i] = gh_scm2int(scm_vector_ref(brush_data, SCM_MAKINUM(i)));
    }

  return brush;
}

SCM
editor_objectmap_get_objects()
{
  SCM lst = SCM_EOL;

  for(EditorObjMap::Objs::iterator i = editor_get_objmap()->get_objects()->begin();
      i != editor_get_objmap()->get_objects()->end();
      ++i)
    {
      lst = gh_cons(SCM_MAKINUM((*i)->handle), lst);
    }

  return gh_reverse(lst);
}

SCM
editor_objectmap_get_object(int id)
{
  EditorObjMap::Obj* obj = editor_get_objmap()->get_object(id);

  if (obj)
    return obj2scm(*obj);
  else
    return SCM_BOOL_F;
}

void
editor_resize_map(int w, int h, int x, int y)
{
  if (EditorTileMap::current())
    {
      EditorTileMap::current()->resize(w, h, x, y);
    }
  else
    {
      assert(!"Error: Tilemap not found");
    }
}

void 
tilemap_paint_tool_set_brush(SCM brush)
{
  TileMapPaintTool::current()->set_brush(scm2brush(brush));
}

SCM
editor_get_tile_selection()
{
  TileMapSelectTool* tool 
    = dynamic_cast<TileMapSelectTool*>(EditorMap::current()->get_tool_by_name(1));

  if (tool)
    {
      return brush2scm(tool->get_selection());
    }
  else
    return SCM_BOOL_F;
}

int
editor_get_brush_tile()
{
  // FIXME: replace this with a tile selector widget in the tile editor
  //return editor_get_tilemap()->brush_tile;
  return 0;
}

void 
editor_tilemap_draw_brush(int pos_x, int pos_y, SCM brush)
{
  // FIXME: Integrate this with tools probally
  EditorTileMap* tilemap = editor_get_tilemap();

  int brush_width  = gh_scm2int(gh_car(brush));
  int brush_height = gh_scm2int(gh_cadr(brush));
  SCM brush_data   = gh_caddr(brush);

  assert(brush_width*brush_height == static_cast<int>(gh_vector_length(brush_data)));

  Field<int>* field = tilemap->get_field();

  int start_x = std::max(0, -pos_x);
  int start_y = std::max(0, -pos_y);

  int end_x = std::min(brush_width,  field->get_width()  - pos_x);
  int end_y = std::min(brush_height, field->get_height() - pos_y);

  for (int y = start_y; y < end_y; ++y)
    for (int x = start_x; x < end_x; ++x)
      {
        field->at(pos_x + x, pos_y + y) 
          = gh_scm2int(scm_vector_ref(brush_data, SCM_MAKINUM(x + (y * brush_width))));
      }
}

int
screen_get_width()
{
  return CL_Display::get_width();
}

int
screen_get_height()
{
  return CL_Display::get_height();
}

void tilemap_set_active_layer(int i)
{
  editor_get_tilemap()->set_active_layer(i);
}

void editor_set_tool(int i)
{
  EditorMap::current()->set_tool(i);
}

CL_Component*
editor_create_map(int x, int y, int w, int h)
{
  return new EditorMap(CL_Rect(CL_Point(x, y),
                               CL_Size(w, h)),
                       GUIManager::current()->get_component());
}

CL_Component*
minimap_create(CL_Component* p, int x, int y, int w, int h)
{
  EditorMap* parent_map = dynamic_cast<EditorMap*>(p);
  return new Minimap(parent_map, 
                     CL_Point(x, y), CL_Size(w, h), 
                     GUIManager::current()->get_component());
}

CL_Component*
object_selector_create(int x, int y, int w, int h, int obj_w, int obj_h)
{
  ObjectSelector* ret = new ObjectSelector(CL_Point(x, y),
                                           w, h, obj_w, obj_h,
                                           GUIManager::current()->get_component());
  return ret;
}

CL_Component*
tile_selector_create(int x, int y, int w, int h, float scale)
{
  TileSelector* ret = new TileSelector(w, h, GUIManager::current()->get_component());
  ret->set_scale(scale);
  return ret;
}

SCM diamond_map_get_data()
{
  Field<int>* field = editor_get_tilemap()->get_diamond_map();
  
  if (field)
    {
      SCM vec = SCM_EOL;
      for (Field<int>::iterator i = field->begin(); i != field->end(); ++i)
        vec = gh_cons(gh_int2scm(*i), vec);

      return gh_reverse(vec);
    }
  else
    {
      return SCM_EOL;
    }
}

SCM map_get_data(int i)
{
  Field<int>* field = editor_get_tilemap()->get_map(i);
  if (field)
    {
      std::cout << ": " << field->get_width() << "x" << field->get_height() 
                << " " << field->size() << std::endl;

      SCM vec = SCM_EOL;
      for (Field<int>::iterator i = field->begin(); i != field->end(); ++i)
        {
          vec = gh_cons(gh_int2scm(*i), vec);
        }
      return gh_reverse(vec);
    }
  else

    {
      return SCM_BOOL_F;
    }
}

int map_get_width()
{
  return editor_get_tilemap()->get_width();
}

int map_get_height()
{
  return editor_get_tilemap()->get_height();
}

void map_set_size(int w, int h)
{
  //return editor_get_tilemap()->get_height();
}

void map_resize(int w, int h)
{
}

void map_clear()
{
}

void
editor_load(const char* filename)
{
  editor_get_tilemap()->load(filename);
}

void
editor_new(int w, int h)
{
  editor_get_tilemap()->new_level(w, h);
}

void
game_play(const char* filename)
{
  std::cout << "WindstilleGame: Starting level " << filename << std::endl;
  WindstilleGame game (filename);
  game.display ();
}

CL_Component*
editor_add_tileeditor(int x, int y)
{
  CL_Component* manager = GUIManager::current()->get_component();
  return new TileEditor(x, y, manager);
}

void tileeditor_set_tile(CL_Component* comp, int id)
{
  TileEditor* tileeditor = dynamic_cast<TileEditor*>(comp);
  if (tileeditor)
    tileeditor->set_tile(TileFactory::current()->create(id));
}

SCM get_tile_def(Tile* tile)
{
  SCM lst = gh_cons(scm_str2symbol("tile"), SCM_EOL);

  if (tile)
    {
      lst = gh_cons(gh_list(scm_str2symbol("id"), SCM_MAKINUM(tile->id), (SCM_UNDEFINED)),
                    lst);

      lst = gh_cons(gh_list(scm_str2symbol("image"), gh_str02scm(tile->filename.c_str()), (SCM_UNDEFINED)),
                    lst);

      lst = gh_cons(gh_list(scm_str2symbol("colmap"), 
                            SCM_MAKINUM(tile->colmap[0]),
                            SCM_MAKINUM(tile->colmap[1]),
                            SCM_MAKINUM(tile->colmap[2]),
                            SCM_MAKINUM(tile->colmap[3]),
                            SCM_MAKINUM(tile->colmap[4]),
                            SCM_MAKINUM(tile->colmap[5]),
                            SCM_MAKINUM(tile->colmap[6]),
                            SCM_MAKINUM(tile->colmap[7]),
                            SCM_UNDEFINED),
                    lst);
    }
  
  return gh_reverse(lst);
}

SCM get_tile_def(int id)
{
  return get_tile_def(TileFactory::current()->create(id));
}

SCM get_tile_defs()
{
  SCM lst = gh_cons(scm_str2symbol("windstille-tiles"), SCM_EOL);
  
  for (TileFactory::iterator i = TileFactory::current()->begin();
       i != TileFactory::current()->end();
       ++i)
    {
      lst = gh_cons(get_tile_def((*i).second), lst);
    }

  return gh_reverse(lst);
}

SCM map_get_scripts()
{
  SCM lst = SCM_EOL;

  std::vector<std::string> scripts = EditorMap::current()->get_scripts();
  for (std::vector<std::string>::iterator i = scripts.begin(); 
       i != scripts.end(); ++i)
    {
      lst = gh_cons(gh_str02scm(i->c_str()), lst);
    }
  
  return gh_reverse(lst);
}

/* EOF */
