SUBDIRS = scripting

bin_PROGRAMS = flexlay averagecolor

averagecolor_SOURCES  = averagecolor.cxx
averagecolor_CPPFLAGS = @EDITOR_CFLAGS@
averagecolor_LDADD    = @EDITOR_LIBS@

flexlay_CPPFLAGS = -I.. @EDITOR_CFLAGS@
flexlay_LDADD    = scripting/libeditor_scripting.a @EDITOR_LIBS@
flexlay_SOURCES  = \
  editor_main.hxx editor_main.cxx \
  tile.hxx tile.cxx \
  scm_functor.hxx scm_functor.cxx \
  scm_helper.hxx  scm_helper.cxx \
  globals.hxx  globals.cxx \
  tileset.hxx  tileset.cxx \
  editor_tilemap.hxx editor_tilemap.cxx \
  tile_selector.hxx tile_selector.cxx \
  editor.hxx editor.cxx \
  editor_objmap.hxx   editor_objmap.cxx \
  editor_map.hxx editor_map.cxx \
  editor_grid_layer.hxx editor_grid_layer.cxx \
  editor_map_component.hxx editor_map_component.cxx \
  tile_editor.hxx  tile_editor.cxx \
  tilemap_tool.hxx tilemap_tool.cxx \
  tilemap_paint_tool.hxx tilemap_paint_tool.cxx \
  tilemap_select_tool.hxx tilemap_select_tool.cxx \
  tile_brush.hxx   tile_brush.cxx \
  minimap.hxx   minimap.cxx \
  editor_names.hxx \
  object_brush.hxx object_brush.cxx \
  object_selector.hxx object_selector.cxx \
  tile_selection.hxx tile_selection.cxx \
  command.hxx \
  gui_manager.hxx gui_manager.cxx \
  paint_command.hxx   paint_command.cxx \
  tool_manager.hxx tool_manager.cxx \
  popup_menu.hxx popup_menu.cxx \
  objmap_object.hxx   objmap_object.cxx \
  objmap_sprite_object.hxx   objmap_sprite_object.cxx \
  object_move_command.hxx  object_move_command.cxx \
  object_delete_command.hxx  object_delete_command.cxx \
  object_add_command.hxx  object_add_command.cxx \
  command_group.hxx command_group.cxx \
  objmap_select_tool.hxx objmap_select_tool.cxx \
  object_transform_command.hxx  object_transform_command.cxx \
  graphic_context_state.cxx graphic_context_state.hxx

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
