SUBDIRS = editor guile

bindir=$(prefix)/games

bin_PROGRAMS = windstille

windstille_SOURCES = \
  controller.hxx gameobj.hxx  windstille_main.hxx \
  delta_manager.hxx  game_world.hxx  player.hxx  \
  player.cxx  windstille_main.cxx game_world.cxx gameobj.cxx \
  default_shoot.hxx   default_shoot.cxx \
  Keyboardcontroller.hxx   keyboard_controller.cxx \
  tile_map.hxx tile_map.cxx field.hxx windstille_game.cxx windstille_game.hxx \
  animation_obj.hxx   animation_obj.cxx dog.hxx dog.cxx \
  power_up.hxx power_up.cxx windstille_level.hxx windstille_level.cxx \
  string_converter.hxx xmlhelper.hxx xmlhelper.cxx \
  laser_shoot.cxx   laser_shoot.hxx player_view.hxx player_view.cxx \
  bonus_flyer.hxx bonus_flyer.cxx guile_gameobj.hxx guile_gameobj.cxx \
  sprite_smob.hxx sprite_smob.cxx guile_gameobj_factory.hxx guile_gameobj_factory.cxx \
  tile_mapsmob.hxx   tile_mapsmob.cxx gameobj_smob.hxx gameobj_smob.cxx \
  game_worldsmob.hxx game_worldsmob.cxx globals.hxx scmconverter.hxx

LDADD = editor/libwindstille_editor.a


# EOF #