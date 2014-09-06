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
#include <ClanLib/Display/Providers/provider_factory.h>
#include <ClanLib/GUI/component.h>
#include <ClanLib/GUI/button.h>
#include <ClanLib/GUI/window.h>

#include "command.hpp"
#include "paint_command.hpp"
#include "object_move_command.hpp"
#include "object_add_command.hpp"
#include "object_delete_command.hpp"
#include "tile.hpp"
#include "tile_brush.hpp"
#include "meta_data.hpp"
#include "gui/console.hpp"
#include "blitter.hpp"

#include "layer.hpp"
#include "tilemap_layer.hpp"
#include "object_layer.hpp"
#include "onion_skin_layer.hpp"

#include "gui/minimap.hpp"
#include "editor_map.hpp"
#include "workspace.hpp"
#include "tileset.hpp"
#include "gui/editor_map_component.hpp"
#include "flexlay.hpp"
#include "globals.hpp"
#include "gui_manager.hpp"
#include "gui/tile_selector.hpp"
#include "object_brush.hpp"
#include "gui/object_selector.hpp"
#include "gui/icon.hpp"
#include "gui/window.hpp"
#include "gui/panel.hpp"
#include "gui/directory_view.hpp"
#include "gui/menu.hpp"
#include "gui/menubar.hpp"
#include "graphic_context_state.hpp"
#include "gui/button_panel.hpp"
#include "gui/file_dialog.hpp"
#include "gui/generic_dialog.hpp"

#include "tools/workspace_move_tool.hpp"
#include "tools/layer_move_tool.hpp"
#include "tools/sketch_stroke_tool.hpp"
#include "sketch_layer.hpp"
#include "bitmap_layer.hpp"
#include "stroke.hpp"
#include "stroke_drawer.hpp"
#include "drawer_properties.hpp"
#include "sprite_stroke_drawer.hpp"
#include "marker_stroke_drawer.hpp"
#include "brushmask.hpp"
#include "brush.hpp"
#include "generated_brush.hpp"
#include "sprite_brush.hpp"

#include "gui/colorpicker.hpp"
#include "gui/slider.hpp"
#include "tools/tilemap_paint_tool.hpp"
#include "tools/tilemap_select_tool.hpp"
#include "tools/objmap_select_tool.hpp"
#include "objmap_sprite_object.hpp"
#include "objmap_rect_object.hpp"
#include "objmap_object.hpp"
#include "tools/zoom_tool.hpp"
#include "tools/zoom2_tool.hpp"
#include "objmap_path_node.hpp"

#include "math/rect.hpp"
#include "math/size.hpp"
#include "math/point.hpp"
#include "math/origin.hpp"

#include "property_value.hpp"

#include "color.hpp"

// #include "netpanzer.hpp" 
#include "helper.hpp"

#ifdef SWIGRUBY
#include "../ruby/ruby_sexpr_parser.hpp"
#include "../ruby/ruby_meta_data.hpp"
#include "../ruby/ruby_functor.hpp"

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

VALUE CL_Color2Value(const Color& arg)
{
 Color* resultptr = new Color(arg);
 return SWIG_NewPointerObj((void *) resultptr, SWIGTYPE_p_Color, 1);
}

VALUE Pointf2Value(const Pointf& arg)
{
 Pointf* resultptr = new Pointf(arg);
 return SWIG_NewPointerObj((void *) resultptr, SWIGTYPE_p_Pointf, 1);
}

VALUE Point2Value(const Point& arg)
{
 Point* resultptr = new Point(arg);
 return SWIG_NewPointerObj((void *) resultptr, SWIGTYPE_p_Point, 1);
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
%template(Std_vector_PropertyValue) std::vector<PropertyValue>;

%typemap(in) std::function<void()> {
    $1 = RubyFunctor($input);
}

%typemap(in) std::function<void(std::string)> {
    $1 = RubyFunctor($input);
}

%include "clanlib.i"
%include "command.hpp"
%include "paint_command.hpp"
%include "object_move_command.hpp"
%include "object_add_command.hpp"
%include "object_delete_command.hpp"
%include "tile.hpp"
%include "tile_brush.hpp"
%include "meta_data.hpp"
%include "gui/console.hpp"
%include "blitter.hpp"
 
%include "layer.hpp"
%include "tilemap_layer.hpp"
%include "object_layer.hpp"
%include "onion_skin_layer.hpp"

%include "editor_map.hpp"
%include "workspace.hpp"
%include "tileset.hpp"
%include "gui/editor_map_component.hpp"
%include "flexlay.hpp"
%include "globals.hpp"
%include "gui_manager.hpp"
%include "gui/tile_selector.hpp"
%include "object_brush.hpp"
%include "gui/object_selector.hpp"
%include "gui/icon.hpp"
%include "gui/window.hpp"
%include "gui/panel.hpp"
%include "gui/minimap.hpp"
%include "gui/directory_view.hpp"
%include "gui/menu.hpp"
%include "gui/menubar.hpp"

%include "gui/button_panel.hpp"
%include "gui/file_dialog.hpp"
%include "gui/generic_dialog.hpp"

%include "tools/workspace_move_tool.hpp"
%include "tools/layer_move_tool.hpp"
%include "tools/sketch_stroke_tool.hpp"
%include "sketch_layer.hpp"
%include "bitmap_layer.hpp"
%include "stroke.hpp"
%include "stroke_drawer.hpp"
%include "drawer_properties.hpp"
%include "sprite_stroke_drawer.hpp"
%include "marker_stroke_drawer.hpp"
%include "brushmask.hpp"
%include "brush.hpp"
%include "generated_brush.hpp"
%include "sprite_brush.hpp"

%include "gui/colorpicker.hpp"
%include "gui/slider.hpp"
%include "tools/tilemap_paint_tool.hpp"
%include "tools/tilemap_select_tool.hpp"
%include "tools/objmap_select_tool.hpp"
%include "objmap_sprite_object.hpp"
%include "objmap_rect_object.hpp"
%include "objmap_object.hpp"
%include "tools/zoom_tool.hpp" 
%include "tools/zoom2_tool.hpp" 
%include "graphic_context_state.hpp"
%include "objmap_path_node.hpp"
# %include "scripting/editor.hpp"

%include "math/rect.hpp"
%include "math/size.hpp"
%include "math/point.hpp"
%include "math/origin.hpp"

%include "property_value.hpp"

%include "color.hpp"

// %include "netpanzer.hpp" 
%include "helper.hpp"

#ifdef SWIGRUBY
%include "../ruby/ruby_meta_data.hpp"
%include "../ruby/ruby_sexpr_parser.hpp"
#endif

// 8

/* EOF */
