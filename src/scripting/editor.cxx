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
#include <ClanLib/core.h>
#include <ClanLib/Display/palette.h>
#include <ClanLib/Display/display.h>
#include <ClanLib/Display/display_window.h>
#include <iostream>
#include <fstream>
#include "../scm_functor.hxx"
#include "../globals.hxx"
#include "../windstille_game.hxx"
#include "../scm_obj.hxx"
#include "editor/editor.hxx"
#include "editor/tile_selector.hxx"
#include "editor/objmap_select_tool.hxx"
#include "editor/editor_tilemap.hxx"
#include "editor/object_selector.hxx"
#include "editor/editor_map.hxx"
#include "editor/editor_map_component.hxx"
#include "editor/tile_editor.hxx"
#include "editor/tilemap_select_tool.hxx"
#include "editor/objmap_object.hxx"
#include "editor/objmap_sprite_object.hxx"
#include "editor/tilemap_paint_tool.hxx"
#include "editor/minimap.hxx"
#include "editor/editor_names.hxx"
#include "tile_factory.hxx"
#include "tile.hxx"
#include "editor/tool_manager.hxx"
#include "editor/object_delete_command.hxx"
#include "editor/object_add_command.hxx"
#include "gui_manager.hxx"

#include "../editor/editor_map.hxx"
#include "../editor/editor_map_layer.hxx"
#include "../editor/editor_objmap.hxx"
#include "../editor/editor_tilemap.hxx"

#include "editor.hxx"

SCM component2scm(CL_Component* comp);

extern CL_ResourceManager* resources;


std::vector<int>
scm2vector(SCM lst)
{
  std::vector<int> ret;
  while (!gh_null_p(lst)) {
    ret.push_back(gh_scm2int(gh_car(lst)));
    lst = gh_cdr(lst);
  }
  return ret;
}

SCM vector2scm(const std::vector<int>& vec)
{
  SCM lst = SCM_EOL;
  for(std::vector<int>::const_iterator i = vec.begin(); i != vec.end(); ++i) {
    lst = gh_cons(SCM_MAKINUM(*i), lst);
  }
  return gh_reverse(lst);
}

void set_window_title(const char* name)
{
  CL_Display::get_current_window()->set_title(name);
}

void editor_redo()
{
  Editor::current()->redo();
}

void editor_undo()
{
  Editor::current()->undo();
}

EditorObjMap*
editor_get_objmap()
{
  return dynamic_cast<EditorObjMap*>(EditorMapComponent::current()->get_map()->get_layer_by_name(OBJECTMAP_NAME));
}

EditorTileMap*
editor_get_tilemap()
{
  EditorTileMap* tilemap 
    = dynamic_cast<EditorTileMap*>(EditorMapComponent::current()->get_map()->get_layer_by_name(TILEMAP_NAME));

  if (tilemap)
    return tilemap;
  else
    {
      assert(!"Error: Tilemap not found");
      return 0;
    }
}

int
objectmap_add_object(EditorMapLayer* obj, const char* name, int x, int y, SCM userdata)
{
  EditorObjMap* objmap = dynamic_cast<EditorObjMap*>(obj);

  if (objmap)
    {
      ObjMapObject* obj 
        = new ObjMapSpriteObject(objmap->get_next_object_handle(), 
                                 CL_Point(x, y), 
                                 SCMObj(userdata), 
                                 CL_Sprite(name, resources));

      ObjectAddCommand* command = new ObjectAddCommand(objmap, obj);
      Editor::current()->execute(command);
      
      return command->get_handle();
    }
  else
    {
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

void
editor_toggle_attributes()
{
  editor_get_tilemap()->set_draw_attribute(!editor_get_tilemap()->get_draw_attribute());
}

SCM
obj2scm(const EditorObjMap::Obj& obj)
{
  SCM lst = SCM_EOL;
  
  return gh_list(gh_long2scm(obj.get_pos().x),
                 gh_long2scm(obj.get_pos().y),
                 obj.get_data().get_scm(),
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

int
editor_objectmap_duplicate_object(int id)
{
  return editor_get_objmap()->duplicate_object(id);
}

void
editor_objectmap_delete_objects(SCM lst)
{
  ObjectDeleteCommand* command = new ObjectDeleteCommand(editor_get_objmap());
  
  std::vector<int> selection = scm2vector(lst);
  for(std::vector<int>::const_iterator i = selection.begin(); i != selection.end(); ++i) {
    command->add_object(*i);
  }
  Editor::current()->execute(command);
}

void
tilemap_object_tool_set_objects(SCM lst)
{
  ObjMapSelectTool::Selection selection;
  ObjMapSelectTool* tool 
    = dynamic_cast<ObjMapSelectTool*>
    (Editor::current()->get_tool_manager()->get_tool_by_name(OBJECT_TOOL_NAME));

  while (!gh_null_p(lst))
    {
      ObjMapObject* obj = editor_get_objmap()->get_object(gh_scm2int(gh_car(lst)));

      if (obj)
        selection.push_back(obj);
      else
        std::cout << "Invalide handle: " << std::endl;

      lst = gh_cdr(lst);
    }
  
  tool->set_selection(selection);
}

void
editor_objectmap_set_pos(int id, int x, int y)
{
  ObjMapObject* obj = editor_get_objmap()->get_object(id);
  obj->set_pos(CL_Point(x, y));
}

SCM
editor_objectmap_get_objects()
{
  SCM lst = SCM_EOL;

  for(EditorObjMap::Objs::iterator i = editor_get_objmap()->get_objects()->begin();
      i != editor_get_objmap()->get_objects()->end();
      ++i)
    {
      lst = gh_cons(SCM_MAKINUM((*i)->get_handle()), lst);
    }

  return gh_reverse(lst);
}

void
tilemap_object_tool_clear_selection()
{
  ObjMapSelectTool* tool 
    = dynamic_cast<ObjMapSelectTool*>
    (Editor::current()->get_tool_manager()->get_tool_by_name(OBJECT_TOOL_NAME));

  tool->clear_selection();
}

SCM
tilemap_object_tool_get_objects()
{
  SCM lst = SCM_EOL;

  ObjMapSelectTool* tool 
    = dynamic_cast<ObjMapSelectTool*>
    (Editor::current()->get_tool_manager()->get_tool_by_name(OBJECT_TOOL_NAME));

  ObjMapSelectTool::Selection selection = tool->get_selection();

  for(EditorObjMap::Objs::iterator i = selection.begin(); i != selection.end(); ++i)
    {
      lst = gh_cons(SCM_MAKINUM((*i)->get_handle()), lst);
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
objmap_sprite_object_flip(int id)
{
  ObjMapSpriteObject* obj = dynamic_cast<ObjMapSpriteObject*>(editor_get_objmap()->get_object(id));
  if (obj)
    obj->flip_horizontal();
}

void tilemap_resize(EditorMapLayer* m, int x, int y, int w, int h)
{
  EditorTileMap* tilemap = dynamic_cast<EditorTileMap*>(m);
  if (tilemap)
    tilemap->resize(w, h, x, y);  
}

void
editor_resize_map(int w, int h, int x, int y)
{
  EditorTileMap* tilemap 
    = dynamic_cast<EditorTileMap*>(EditorMapComponent::current()->get_map()->get_layer_by_name(TILEMAP_NAME));

  if (tilemap)
    {
      tilemap->resize(w, h, x, y);
    }
  else
    {
      assert(!"Error: Tilemap not found");
    }
}

void 
tilemap_paint_tool_set_brush(TileBrush brush)
{
  TileMapPaintTool::current()->set_brush(brush);
}

SCM
editor_get_tile_selection()
{
  TileMapSelectTool* tool 
    = dynamic_cast<TileMapSelectTool*>(Editor::current()->get_tool_manager()->get_tool_by_name(1));

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
  const TileBrush& brush = TileMapPaintTool::current()->get_brush();
  if (brush.get_width() > 0 && brush.get_height() > 0)
    return brush(0, 0);
  else
    return 0;
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
  Editor::current()->get_tool_manager()->set_tool(i);
}

CL_Component*
editor_map_component_create(int x, int y, int w, int h)
{
  return new EditorMapComponent(CL_Rect(CL_Point(x, y),
                                        CL_Size(w, h)),
                                GUIManager::current()->get_component());
}

CL_Component*
minimap_create(CL_Component* p, int x, int y, int w, int h)
{
  EditorMapComponent* parent_map = dynamic_cast<EditorMapComponent*>(p);
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
      lst = gh_cons(get_tile_def(i - TileFactory::current()->begin()), lst);
    }

  return gh_reverse(lst);
}

SCM map_get_scripts()
{
  SCM lst = SCM_EOL;

  std::vector<std::string> scripts = EditorMapComponent::current()->get_map()->get_scripts();
  for (std::vector<std::string>::iterator i = scripts.begin(); 
       i != scripts.end(); ++i)
    {
      lst = gh_cons(gh_str02scm(i->c_str()), lst);
    }
  
  return gh_reverse(lst);
}

SCM
xml_node2scm(CL_DomNode& root)
{
  /*
   * (element ("foo" (("name" . "Paul")))
   *          (element ("bar" ())
   *                   (character-data "Some text"))
   *          (element ("void" ()))) 
   */
  if (root.is_element())
    {
      CL_DomElement element = root.to_element();
      
      // Construct attributes
      SCM attributes = SCM_EOL;
      std::cout << "Attributes: " << root.get_attributes().get_length() << std::endl;
      CL_DomNamedNodeMap attrs = root.get_attributes();
      for(int i = 0; i < attrs.get_length(); ++i)
        {
          if (attrs.item(i).is_attr())
            {
              CL_DomAttr attr = attrs.item(i).to_attr();
              attributes = gh_cons(gh_cons(gh_str02scm(attr.get_name().c_str()),
                                           gh_str02scm(attr.get_value().c_str())),
                                   attributes);
            }
        }
      attributes = gh_reverse(attributes);

      // Construct children
      SCM children   = SCM_EOL;
      CL_DomNode child = root.get_first_child();
      while(!child.is_null())
        {
          children = gh_cons(xml_node2scm(child), children);
          child = child.get_next_sibling();
        }
      children = gh_reverse(children);
      
      // Assemble the whole element
      return gh_cons(gh_symbol2scm("element"), 
                     gh_cons(gh_cons(gh_str02scm(element.get_tag_name().c_str()),
                                     attributes),
                             children));
    }
  else if (root.is_null())
    {
      return SCM_EOL;
    }
  else if (root.is_element())
    {
    }
  else if (root.is_attr())
    {
    }
  else if (root.is_text())
    {
      // FIXME: Doesn't seem to work
      CL_DomCDATASection text = root.to_cdata_section();
      return gh_list(gh_symbol2scm("character-data"),
                     gh_str02scm(text.substring_data(0, text.get_length()).c_str()),
                     SCM_UNDEFINED);
    }
  else if (root.is_cdata_section())
    {
    }
  else if (root.is_entity_reference())
    {
    }
  else if (root.is_entity())
    {
    }
  else if (root.is_processing_instruction())
    {
    }
  else if (root.is_comment())
    {
    }
  else if (root.is_document())
    {
    }
  else if (root.is_document_type())
    {
    }
  else if (root.is_document_fragment())
    {
    }
  else if (root.is_notation())
    {
    }
  else
    {
      std::cout << "Error: Unhandled node type" << std::endl;
    }

  return SCM_BOOL_F;
}

SCM
load_xml(const char* filename)
{
  try {
    CL_DomDocument document;
    document.load(new CL_InputSource_File(filename), true, true);
    CL_DomElement root = document.get_document_element();
    return xml_node2scm(root);
  } catch (CL_Error& err) {
    std::cout << "CL_Error: " << err.message << std::endl;
    return SCM_BOOL_F;
  }
}

EditorMap*
editor_map_component_get_map(CL_Component* c)
{
  EditorMapComponent* parent_map = dynamic_cast<EditorMapComponent*>(c); 
  return parent_map->get_map();
}

void
editor_map_component_set_map(CL_Component* c, EditorMap* m)
{
  EditorMapComponent* parent_map = dynamic_cast<EditorMapComponent*>(c); 
  parent_map->set_map(m);
}

// Map stuff
EditorMap*
editor_map_create()
{
  return new EditorMap("");
}

void
editor_map_add_layer(EditorMap* m, EditorMapLayer* layer)
{
  m->add_layer(layer);
}

EditorMapLayer* 
editor_objmap_create()
{
  return new EditorObjMap();
}

EditorMapLayer* 
editor_tilemap_create(int w, int h, int tile_size)
{
  return new EditorTileMap(w, h, tile_size);
}

void
editor_tilemap_set_data(EditorMapLayer* l, int m, SCM lst)
{
  EditorTileMap* tilemap = dynamic_cast<EditorTileMap*>(l);
  if (tilemap)
    {
      Field<int>* field = tilemap->get_map(m);
      for(Field<int>::iterator i = field->begin(); 
          i != field->end() && gh_pair_p(lst);
          ++i)
        {
          (*i) = gh_scm2int(gh_car(lst));
          lst = gh_cdr(lst);
        }
    }
}

std::string
editor_map_get_filename(EditorMap* m)
{
  return m->get_filename();
}

bool
editor_map_is_modified(EditorMap* m)
{
  return m->is_modified();
}

void
editor_map_set_unmodified(EditorMap* m)
{
  m->set_unmodified();
}

void
editor_map_set_filename(EditorMap* m, const char* name)
{
  return m->set_filename(name);
}


/*
std::string scm2string(SCM s)
{
  char* tmp = gh_scm2newstr(s, 0);
  std::string str = tmp;
  free(tmp);
  return tmp;
}*/

SCM string2scm(const std::string& str)
{
  return gh_str02scm(str.c_str());
}

struct MenuConverter
{
  SCMFunctor func;

  MenuConverter(SCM f)
    : func(f)
  {
  }

  void operator()(CL_Menu* menu) {
    func(component2scm(menu));
  }
};

void
objectmap_tool_set_popupmenu_callback(SCM func)
{
  ObjMapSelectTool* tool 
    = dynamic_cast<ObjMapSelectTool*>
    (Editor::current()->get_tool_manager()->get_tool_by_name(OBJECT_TOOL_NAME));
  
  MenuConverter callback(func);

  new CL_Slot(tool->sig_on_popup_menu_display().connect_functor(callback));
}

CL_Palette netpanzer_load_palette(const char* filename)
{
  CL_Palette palette;
  unsigned char color_array[256 * 3];

  std::ifstream in(filename);

  if (!in)
    {
      std::cout << "Couldn't load palette" << std::endl;
      return palette;
    }

  in.read(reinterpret_cast<char*>(color_array), sizeof(color_array));

  for(int i = 0; i < 256; ++i)
    {
      palette.colors[i].set_red  (color_array[3*i + 0]);
      palette.colors[i].set_green(color_array[3*i + 1]);
      palette.colors[i].set_blue (color_array[3*i + 2]);
    }

  return palette;
}

unsigned char find_nearest_color(const CL_Palette& palette, const CL_Color& rgb)
{ // Copyright (C) 1998 Pyrosoft Inc. (www.pyrosoftgames.com), Matthew Bogue
  float bestDist = 10000000.0f;
  int   best     = 0;
                                                                                                                
  float vPic = sqrt(rgb.get_red() * rgb.get_red() 
                    + rgb.get_green() * rgb.get_green()
                    + rgb.get_blue() * rgb.get_blue()) * 0.57735027;
 
  for (int i = 0; i < 256; i++) {
    float vPal = sqrt(palette.colors[i].get_red()     * palette.colors[i].get_red()
                      + palette.colors[i].get_green() * palette.colors[i].get_green()
                      + palette.colors[i].get_blue()  * palette.colors[i].get_blue()) * 0.57735027;
                                                                                                                
    float dr = palette.colors[i].get_red()   - rgb.get_red();
    float dg = palette.colors[i].get_green() - rgb.get_green();
    float db = palette.colors[i].get_blue()  - rgb.get_blue();
    float dv = vPal-vPic;
    float dist = dr * dr * 0.3 + dg * dg * 0.59 + db * db * 0.11 + dv * dv * 0.7;
                                                                                                                
    if (dist < bestDist) {
      bestDist = dist;
      best = i;
    }
  }
                                                                                                                
  return best;
                                                                                                                
}

void
save_netpanzer_map(const char* filename, EditorMap* m, 
                   const char* id_header_, const char* name_, const char* description_)
{
  EditorTileMap* tilemap = dynamic_cast<EditorTileMap*>(m->get_layer_by_name(TILEMAP_NAME));

  if (!tilemap)
    return;
    
  unsigned char   netp_id_header[64];
  strcpy(reinterpret_cast<char*>(netp_id_header), id_header_);
  unsigned short  id       = 0; // ?
  char   name[256];
  strcpy(name, name_);
  char   description[1024];
  strcpy(description, description_);
  unsigned short  x_size   = tilemap->get_width();
  unsigned short  y_size   = tilemap->get_height();
  char            tile_set[256] = "summer12mb.tls";
 
  unsigned short  thumbnail_x_pix = tilemap->get_width();
  unsigned short  thumbnail_y_pix = tilemap->get_height();
    
  std::ofstream out(filename);

  out.write(reinterpret_cast<char*>(&netp_id_header), sizeof(netp_id_header));
  out.write(reinterpret_cast<char*>(&id), sizeof(short));
  out.write(reinterpret_cast<char*>(&name), sizeof(name));
  out.write(reinterpret_cast<char*>(&description), sizeof(description));
  out.write(reinterpret_cast<char*>(&x_size), sizeof(short));
  out.write(reinterpret_cast<char*>(&y_size), sizeof(short));
  out.write(reinterpret_cast<char*>(&tile_set), sizeof(tile_set));
  out.write(reinterpret_cast<char*>(&thumbnail_x_pix), sizeof(short));
  out.write(reinterpret_cast<char*>(&thumbnail_y_pix), sizeof(short));

  std::vector<unsigned short> vec(x_size * y_size);

  Field<int>* field = tilemap->get_map(1);
  for(int i = 0; i < x_size * y_size; ++i)
    {
      vec[i] = (*field)[i];
    }
  out.write(reinterpret_cast<char*>(&(*vec.begin())), 
            sizeof(unsigned short)*vec.size());

  // Generate thumbnail
  CL_Palette palette = netpanzer_load_palette((datadir + "netp.act").c_str());
  
  std::vector<unsigned char> thumbnail(x_size * y_size);
  for(int i = 0; i < int(thumbnail.size()); ++i)
    {
      Tile* tile = TileFactory::current()->create((*field)[i]);
      if (tile)
        thumbnail[i] = find_nearest_color(palette, tile->get_color());
    }

  out.write(reinterpret_cast<char*>(&(*thumbnail.begin())), 
            sizeof(unsigned char)*thumbnail.size());
}

NetPanzerFileStruct*
load_netpanzer_map(const char* filename)
{
  // FIXME: endian issues
  unsigned char   netp_id_header[64]; // Copyright PyroSoft Inc.  ...
  unsigned short  id; // What is this?
  char   name[256];
  char   description[1024];
  unsigned short  x_size; // width
  unsigned short  y_size; // height
  char            tile_set[256]; // name of the tileset: "summer12mb.tls"
 
  unsigned short  thumbnail_x_pix;
  unsigned short  thumbnail_y_pix;

  std::ifstream file(filename);

  if (!file)
    return 0;

  file.read(reinterpret_cast<char*>(&netp_id_header), sizeof(netp_id_header));
  file.read(reinterpret_cast<char*>(&id), sizeof(short));
  file.read(reinterpret_cast<char*>(&name), sizeof(name));
  file.read(reinterpret_cast<char*>(&description), sizeof(description));
  file.read(reinterpret_cast<char*>(&x_size), sizeof(short));
  file.read(reinterpret_cast<char*>(&y_size), sizeof(short));
  file.read(reinterpret_cast<char*>(&tile_set), sizeof(tile_set));
  file.read(reinterpret_cast<char*>(&thumbnail_x_pix), sizeof(short));
  file.read(reinterpret_cast<char*>(&thumbnail_y_pix), sizeof(short));

  EditorTileMap* tilemap = new EditorTileMap(x_size, y_size, 32);
  Field<int>* field      = tilemap->get_map(1);

  std::vector<unsigned short> vec;
  vec.resize(x_size * y_size);
  file.read(reinterpret_cast<char*>(&(*vec.begin())), sizeof(unsigned short)*vec.size());

  for(int i = 0; i < x_size*y_size; ++i)
    (*field)[i] = vec[i];

  std::cout << "Thumbnail: " << thumbnail_x_pix << " " << thumbnail_y_pix << std::endl;

  NetPanzerFileStruct* netpanzer_file = new NetPanzerFileStruct;

  netpanzer_file->tilemap     = tilemap;
  netpanzer_file->id_header   = reinterpret_cast<char*>(netp_id_header);
  netpanzer_file->name        = name;
  netpanzer_file->description = description;

  return netpanzer_file;
}

void
editor_map_set_metadata(EditorMap* m, SCM data)
{
  return m->set_metadata(SCMObj(data));
}

SCM
editor_map_get_metadata(EditorMap* m)
{
  return m->get_metadata().get_scm();
}

/* EOF */
