SUBDIRS = editor guile collision scripting guistyle

bindir=$(prefix)/games

bin_PROGRAMS = windstille

windstille_SOURCES = \
  animation_obj.cxx \
  animation_obj.hxx \
  background.hxx \
  background.cxx \
  bomb.hxx \
  bomb.cxx \
  bonus_flyer.cxx \
  bonus_flyer.hxx \
  controller.hxx \
  controller.cxx \
  diamond_map.hxx \
  diamond_map.cxx \
  default_shoot.cxx \
  default_shoot.hxx \
  dialog_manager.hxx \
  dialog_manager.cxx \
  delta_manager.hxx \
  display.hxx \
  display.cxx \
  dog.cxx \
  dog.hxx \
  energiebar.hxx \
  energiebar.cxx \
  field.hxx \
  fonts.hxx \
  fonts.cxx \
  gamepad_controller.cxx \
  gamepad_controller.hxx \
  game_delta.hxx \
  game_world.cxx \
  game_world.hxx \
  gameobj.cxx \
  gameobj.hxx \
  globals.hxx \
  globals.cxx \
  guile_gameobj.cxx \
  guile_gameobj.hxx \
  guile_gameobj_factory.cxx \
  guile_gameobj_factory.hxx \
  gui_manager.hxx \
  gui_manager.cxx \
  igel.hxx \
  igel.cxx \
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
  scm_functor.hxx \
  scm_functor.cxx \
  scm_helper.cxx \
  scm_helper.hxx \
  scmconverter.hxx \
  sprite.hxx \
  screen.hxx \
  screen.cxx \
  string_converter.hxx \
  tile.cxx \
  tile.hxx \
  tile_factory.cxx \
  tile_factory.hxx \
  tile_map.cxx \
  tile_map.hxx \
  trigger.hxx \
  trigger.cxx \
  water_map.cxx \
  water_map.hxx \
  windstille_game.cxx \
  windstille_game.hxx \
  windstille_level.cxx \
  windstille_level.hxx \
  windstille_main.cxx \
  windstille_main.hxx \
  windstille_menu.hxx \
  windstille_menu.cxx \
  windstille_error.hxx \
  windstille_error.cxx \
  view.hxx \
  view.cxx \
  view_component.hxx \
  view_component.cxx \
  water_splash.hxx \
  water_splash.cxx

windstille_LDADD = \
  scripting/libwindstille_scripting.a \
  editor/libwindstille_editor.a \
  collision/libwindstille_collision.a \
  guistyle/libwindstille_guistyle.a \
  @WINDSTILLE_LIBS@

windstille_CPPFLAGS = \
  @WINDSTILLE_CFLAGS@

REST_LIBS = \
 -lpthread \
 -lc \
 -lgcc \
 -Wl,-dynamic-linker=/lib/ld-linux.so.2 \
 -Wl,-Bdynamic \
 -lGL

REST_LIBS2 = \
-Wl,-Bdynamic \
-lGL \
-lc

STATICBIN_LIBS = \
-L/usr/X11R6/lib/ \
-Wl,-Bstatic \
 @WINDSTILLE_LIBS@ \
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
$(REST_LIBS2)

static: windstille.static

windstille.static: $(windstille_OBJECTS) $(windstille_DEPENDENCIES) Makefile
	$(CXX) -s -nodefaultlibs $(CXXFLAGS)   @WINDSTILLE_CFLAGS@ -o $@ $(windstille_OBJECTS) \
         $(windstille_DEPENDENCIES) $(STATICBIN_LIBS) 

windstille.static.comp: windstille.static
	cp $< $@
	strip -s $@
	upx-ucl -q --best $@

# EOF #
