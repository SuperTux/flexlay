SUBDIRS = scripting

bin_PROGRAMS = flexlay averagecolor

averagecolor_SOURCES  = averagecolor.cxx
averagecolor_CPPFLAGS = @EDITOR_CFLAGS@
averagecolor_LDADD    = @EDITOR_LIBS@

flexlay_CPPFLAGS = -I.. @EDITOR_CFLAGS@
flexlay_LDADD    = scripting/libeditor_scripting.a @EDITOR_LIBS@
flexlay_SOURCES  = \
  command.hxx  \
  command_group.cxx  \
  command_group.hxx  \
  editor.cxx  \
  editor.hxx  \
  editor_grid_layer.cxx  \
  editor_grid_layer.hxx  \
  editor_main.cxx  \
  editor_main.hxx  \
  editor_map.cxx  \
  editor_map.hxx  \
  editor_map_component.cxx  \
  editor_map_component.hxx  \
  editor_map_layer.hxx  \
  editor_mapsize_layer.cxx  \
  editor_mapsize_layer.hxx  \
  editor_names.hxx  \
  editor_objmap.cxx  \
  editor_objmap.hxx  \
  editor_tilemap.cxx  \
  editor_tilemap.hxx  \
  field.hxx  \
  globals.cxx  \
  globals.hxx  \
  graphic_context_state.cxx  \
  graphic_context_state.hxx  \
  gui_manager.cxx  \
  gui_manager.hxx  \
  math.hxx  \
  minimap.cxx  \
  minimap.hxx  \
  netpanzer  \
  object_add_command.cxx  \
  object_add_command.hxx  \
  object_brush.cxx  \
  object_brush.hxx  \
  object_delete_command.cxx  \
  object_delete_command.hxx  \
  object_move_command.cxx  \
  object_move_command.hxx  \
  object_selector.cxx  \
  object_selector.hxx  \
  object_transform_command.cxx  \
  object_transform_command.hxx  \
  objmap_object.cxx  \
  objmap_object.hxx  \
  objmap_select_tool.cxx  \
  objmap_select_tool.hxx  \
  objmap_sprite_object.cxx  \
  objmap_sprite_object.hxx  \
  paint_command.cxx  \
  paint_command.hxx  \
  popup_menu.cxx  \
  popup_menu.hxx  \
  scm_functor.cxx  \
  scm_functor.hxx  \
  scm_helper.cxx  \
  scm_helper.hxx  \
  scm_obj.hxx  \
  string_converter.hxx  \
  supertux  \
  tile.cxx  \
  tile.hxx  \
  tile_brush.cxx  \
  tile_brush.hxx  \
  tile_editor.cxx  \
  tile_editor.hxx  \
  tile_selection.cxx  \
  tile_selection.hxx  \
  tile_selector.cxx  \
  tile_selector.hxx  \
  tilemap_diamond_tool.cxx  \
  tilemap_diamond_tool.hxx  \
  tilemap_paint_tool.cxx  \
  tilemap_paint_tool.hxx  \
  tilemap_select_tool.cxx  \
  tilemap_select_tool.hxx  \
  tilemap_tool.cxx  \
  tilemap_tool.hxx  \
  tileset.cxx  \
  tileset.hxx  \
  tool_manager.cxx  \
  tool_manager.hxx  \
  workspace.cxx  \
  workspace.hxx  \
  zoom_tool.cxx  \
  zoom_tool.hxx

#  tilemap_diamond_tool.hxx tilemap_diamond_tool.cxx 
STATICBIN_LIBS = \
-L/usr/X11R6/lib/ \
-Wl,-Bstatic \
 @EDITOR_LIBS@ \
-lguile \
-lguile-ltdl \
-lqthreads \
$(STATIC_READLINE) \
-lcrypt \
-lvorbis \
-logg \
-lmikmod \
-ljpeg \
-lpng12 \
-lz \
-lm \
-lX11 \
-lXxf86vm \
-lSM \
-lXt \
-lXext \
-lGLU \
-ldl \
-lX11 \
$(REST_LIBS2) \
-Wl,-Bdynamic \
-lGL

flexlay.semistatic: $(flexlay_OBJECTS) $(flexlay_DEPENDENCIES) Makefile
	$(CXX) -s $(CXXFLAGS)   @EDITOR_CFLAGS@ -o $@ $(flexlay_OBJECTS) \
         $(flexlay_DEPENDENCIES) -Wl,-Bstatic @EDITOR_LIBS@ -Wl,-Bdynamic \
          -lguile -L/usr/X11R6/lib/ -lX11 -lGL -lpng -ljpeg -lXxf86vm -lGLU

flexlay.static: $(flexlay_OBJECTS) $(flexlay_DEPENDENCIES) Makefile
	$(CXX) -s $(CXXFLAGS)   @EDITOR_CFLAGS@ -o $@ $(flexlay_OBJECTS) \
         $(flexlay_DEPENDENCIES) $(STATICBIN_LIBS) 

flexlay.static.comp: flexlay.static
	cp $< $@
	strip -s $@
	upx-ucl -q --best $@

# EOF #
