SUBDIRS = scripting

bin_PROGRAMS = editor averagecolor

averagecolor_SOURCES  = averagecolor.cxx
averagecolor_CPPFLAGS = @EDITOR_CFLAGS@
averagecolor_LDADD    = @EDITOR_LIBS@

editor_CPPFLAGS = -I.. @EDITOR_CFLAGS@
editor_LDADD    = scripting/libeditor_scripting.a @EDITOR_LIBS@
editor_SOURCES = \
  editor_main.hxx editor_main.cxx \
  tile.hxx tile.cxx \
  ../scm_functor.hxx  ../scm_functor.cxx \
  ../scm_helper.hxx  ../scm_helper.cxx \
  ../globals.hxx  ../globals.cxx \
  tile_factory.hxx   tile_factory.cxx \
  editor_tilemap.hxx editor_tilemap.cxx \
  tile_selector.hxx tile_selector.cxx \
  editor.hxx editor.cxx \
  editor_objmap.hxx   editor_objmap.cxx \
  editor_map.hxx editor_map.cxx \
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
  object_transform_command.hxx  object_transform_command.cxx

#  tilemap_diamond_tool.hxx tilemap_diamond_tool.cxx 

editor.static: $(editor_OBJECTS) $(editor_DEPENDENCIES) Makefile
	$(CXX) -s -nodefaultlibs $(CXXFLAGS)   @EDITOR_CFLAGS@ -o $@ $(editor_OBJECTS) \
         $(editor_DEPENDENCIES) $(STATICBIN_LIBS) 

editor.static.comp: editor.static
	cp $< $@
	strip -s $@
	upx-ucl -q --best $@

# EOF #
