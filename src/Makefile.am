SUBDIRS = editor guile

bindir=$(prefix)/games

bin_PROGRAMS = windstille

windstille_SOURCES = \
  animation_obj.cxx \
  animation_obj.hxx \
  bonus_flyer.cxx \
  bonus_flyer.hxx \
  controller.hxx \
  default_shoot.cxx \
  default_shoot.hxx \
  delta_manager.hxx \
  display.hxx \
  display.cxx \
  dog.cxx \
  dog.hxx \
  field.hxx \
  game_world.cxx \
  game_world.hxx \
  game_worldsmob.cxx \
  game_worldsmob.hxx \
  gameobj.cxx \
  gameobj.hxx \
  gameobj_smob.cxx \
  gameobj_smob.hxx \
  globals.hxx \
  guile_gameobj.cxx \
  guile_gameobj.hxx \
  guile_gameobj_factory.cxx \
  guile_gameobj_factory.hxx \
  keyboard_controller.cxx \
  keyboard_controller.hxx \
  laser_shoot.cxx \
  laser_shoot.hxx \
  player.cxx \
  player.hxx \
  player_view.cxx \
  player_view.hxx \
  power_up.cxx \
  power_up.hxx \
  scm_helper.cxx \
  scm_helper.hxx \
  scmconverter.hxx \
  sprite.hxx \
  sprite_smob.cxx \
  sprite_smob.hxx \
  string_converter.hxx \
  tile.cxx \
  tile.hxx \
  tile_factory.cxx \
  tile_factory.hxx \
  tile_map.cxx \
  tile_map.hxx \
  tile_mapsmob.cxx \
  tile_mapsmob.hxx \
  windstille_game.cxx \
  windstille_game.hxx \
  windstille_level.cxx \
  windstille_level.hxx \
  windstille_main.cxx \
  windstille_main.hxx

windstille_LDADD = editor/libwindstille_editor.a

STATICBIN_LIBS = \
-L/usr/X11R6/lib/ \
-Wl,-Bstatic \
-lclanApp \
-lclanVorbis \
-lclanMikMod \
-lclanSound \
-lclanGL \
-lclanDisplay \
-lclanCore \
-lguile \
-lguile-ltdl \
$(STATIC_READLINE) \
-lqthreads \
-lcrypt \
-lHermes \
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
-lstdc++ \
-lgcc_eh \
-lgcc \
-Wl,-Bdynamic \
-lGL \
-lc

static: windstille.static

windstille.static: $(windstille_OBJECTS) $(windstille_DEPENDENCIES) Makefile
	$(CXX) -s -nodefaultlibs $(CXXFLAGS)  -o $@ $(windstille_OBJECTS) \
         $(windstille_DEPENDENCIES) $(STATICBIN_LIBS) 

windstille.static.comp: windstille.static
	cp $< $@
	strip -s $@
	upx-ucl -q --best $@


# EOF #