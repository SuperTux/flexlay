noinst_LIBRARIES = libwindstille_editor.a

libwindstille_editor_a_SOURCES = \
  windstille_wrap.cxx \
  editor.hxx editor.cxx \
  scripting.hxx   scripting.cxx

#  editor_tile.cxx     editor_tilemapview.cxx  windstille_editor.cxx \
#  editor_tile.hxx     editor_tilemapview.hxx  windstille_editor.hxx \
#  editor_tilemap.cxx  tile_mapeditor.hxx \
#  editor_tilemap.hxx  tile_selector.hxx

# Include windstille_wrap.cxx in case the user doesn't have swig installed
EXTRA_DIST = windstille_wrap.cxx

windstille_wrap.cxx: windstille.i  $(command_source)
	$(SWIG) -guile -c++  -o $@ $<

# We override the default compile command to skip all the warnings
# that compiling the wrappers causes.
windstille_wrap.o : windstille_wrap.cxx
	$(CXX) -c -O2 -o $@ $<


# EOF #
