%module flexlay_wrap
 
%exception {
  try {
    $action
  }
  catch (const CL_Error& err) {
//    static VALUE cpperror = rb_define_class("CPPError", rb_eStandardError);
//    rb_raise(cpperror, "Range error.");
std::cout << "CL_Error: " << err.message << std::endl;
  }
  catch (...) {
    std::cout << "Catched some C++ exception" << std::endl;
  }
}
 
%{
#include <ClanLib/Display/color.h>
#include <ClanLib/Display/Providers/provider_factory.h>
#include <ClanLib/GUI/component.h>
#include <ClanLib/GUI/button.h>
#include <ClanLib/GUI/window.h>
#include <ClanLib/Core/Math/rect.h>
#include <ClanLib/Core/Math/point.h>
#include "command.hxx"
#include "paint_command.hxx"
#include "object_move_command.hxx"
#include "object_add_command.hxx"
#include "object_delete_command.hxx"
#include "tile.hxx"
#include "tile_brush.hxx"
#include "editor.hxx"
#include "meta_data.hxx"
#include "console.hxx"
#include "blitter.hxx"

#include "layer.hxx"
#include "tilemap_layer.hxx"
#include "object_layer.hxx"
#include "onion_skin_layer.hxx"

#include "minimap.hxx"
#include "editor_map.hxx"
#include "workspace.hxx"
#include "tileset.hxx"
#include "editor_map_component.hxx"
#include "flexlay.hxx"
#include "globals.hxx"
#include "gui_manager.hxx"
#include "tile_selector.hxx"
#include "object_brush.hxx"
#include "object_selector.hxx"
#include "icon.hxx"
#include "window.hxx"
#include "panel.hxx"
#include "directory_view.hxx"
#include "menu.hxx"
#include "menubar.hxx"
#include "scrollbar.hxx"
#include "graphic_context_state.hxx"
 
#include "layer_move_tool.hxx"
#include "sketch_stroke_tool.hxx"
#include "sketch_layer.hxx"
#include "bitmap_layer.hxx"
#include "stroke.hxx"
#include "stroke_drawer.hxx"
#include "drawer_properties.hxx"
#include "sprite_stroke_drawer.hxx"
#include "marker_stroke_drawer.hxx"
#include "brushmask.hxx"
#include "brush.hxx"
#include "generated_brush.hxx"
#include "sprite_brush.hxx"

#include "colorpicker.hxx"
#include "slider.hxx"
#include "tilemap_paint_tool.hxx"
#include "tilemap_select_tool.hxx"
#include "objmap_select_tool.hxx"
#include "objmap_sprite_object.hxx"
#include "objmap_rect_object.hxx"
#include "objmap_object.hxx"
#include "zoom_tool.hxx"
#include "objmap_path_node.hxx"

// #include "netpanzer.hxx" 
#include "helper.hxx"

#ifdef SWIGRUBY
#include "ruby_sexpr_parser.hxx"
#include "ruby_meta_data.hxx"
#include "ruby_functor.hxx"

VALUE ObjMapObject2Value(const ObjMapObject& arg)
{
 ObjMapObject* resultptr = new ObjMapObject(arg);
 return SWIG_NewPointerObj((void *) resultptr, SWIGTYPE_p_ObjMapObject, 1);
}

VALUE ObjectBrush2Value(const ObjectBrush& arg)
{
 ObjectBrush* resultptr = new ObjectBrush(arg);
 return SWIG_NewPointerObj((void *) resultptr, SWIGTYPE_p_ObjectBrush, 1);
}

VALUE CL_Color2Value(const CL_Color& arg)
{
 CL_Color* resultptr = new CL_Color(arg);
 return SWIG_NewPointerObj((void *) resultptr, SWIGTYPE_p_CL_Color, 1);
}


VALUE CL_Pointf2Value(const CL_Pointf& arg)
{
 CL_Pointf* resultptr = new CL_Pointf(arg);
 return SWIG_NewPointerObj((void *) resultptr, SWIGTYPE_p_CL_Pointf, 1);
}

VALUE CL_Point2Value(const CL_Point& arg)
{
 CL_Point* resultptr = new CL_Point(arg);
 return SWIG_NewPointerObj((void *) resultptr, SWIGTYPE_p_CL_Point, 1);
}
#endif
%}

%include "std_string.i"
%include "std_vector.i"
%template(Std_vector_string) std::vector<std::string>;
%template(Std_vector_int) std::vector<int>;
%template(Std_vector_CL_RadioButton) std::vector<CL_RadioButton *>;
%template(Std_vector_ObjMapObject) std::vector<ObjMapObject>;
%template(Std_vector_CL_Pointf) std::vector<CL_Pointf>;
%template(Std_vector_Stroke) std::vector<Stroke>;
%template(Std_vector_Dab) std::vector<Dab>;

%include "clanlib.i"
%include "command.hxx"
%include "paint_command.hxx"
%include "object_move_command.hxx"
%include "object_add_command.hxx"
%include "object_delete_command.hxx"
%include "tile.hxx"
%include "tile_brush.hxx"
%include "editor.hxx"
%include "meta_data.hxx"
%include "console.hxx"
%include "blitter.hxx"
 
%include "layer.hxx"
%include "tilemap_layer.hxx"
%include "object_layer.hxx"
%include "onion_skin_layer.hxx"

%include "editor_map.hxx"
%include "workspace.hxx"
%include "tileset.hxx"
%include "editor_map_component.hxx"
%include "flexlay.hxx"
%include "globals.hxx"
%include "gui_manager.hxx"
%include "tile_selector.hxx"
%include "object_brush.hxx"
%include "object_selector.hxx"
%include "icon.hxx"
%include "window.hxx"
%include "panel.hxx"
%include "minimap.hxx"
%include "directory_view.hxx"
%include "menu.hxx"
%include "menubar.hxx"
%include "scrollbar.hxx"

%include "layer_move_tool.hxx"
%include "sketch_stroke_tool.hxx"
%include "sketch_layer.hxx"
%include "bitmap_layer.hxx"
%include "stroke.hxx"
%include "stroke_drawer.hxx"
%include "drawer_properties.hxx"
%include "sprite_stroke_drawer.hxx"
%include "marker_stroke_drawer.hxx"
%include "brushmask.hxx"
%include "brush.hxx"
%include "generated_brush.hxx"
%include "sprite_brush.hxx"

%include "colorpicker.hxx"
%include "slider.hxx"
%include "tilemap_paint_tool.hxx"
%include "tilemap_select_tool.hxx"
%include "objmap_select_tool.hxx"
%include "objmap_sprite_object.hxx"
%include "objmap_rect_object.hxx"
%include "objmap_object.hxx"
%include "zoom_tool.hxx" 
%include "graphic_context_state.hxx"
%include "objmap_path_node.hxx"
# %include "scripting/editor.hxx"

// %include "netpanzer.hxx" 
%include "helper.hxx"

#ifdef SWIGRUBY
%include "../ruby/ruby_meta_data.hxx"
%include "../ruby/ruby_sexpr_parser.hxx"
#endif

/* EOF */
