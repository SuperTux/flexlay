SUBDIRS = editor guile

bindir=$(prefix)/games

bin_PROGRAMS = windstille

windstille_SOURCES = \
  Controller.hxx GameObj.hxx GamepadController.hxx WindstilleMain.hxx \
  DeltaManager.hxx  GameWorld.hxx  Player.hxx GamepadController.cxx \
  Player.cxx  WindstilleMain.cxx GameWorld.cxx GameObj.cxx \
  DefaultShoot.hxx   DefaultShoot.cxx \
  KeyboardController.hxx   KeyboardController.cxx \
  TileMap.hxx TileMap.cxx Field.hxx WindstilleGame.cxx WindstilleGame.hxx \
  AnimationObj.hxx   AnimationObj.cxx Dog.hxx Dog.cxx \
  PowerUp.hxx PowerUp.cxx WindstilleLevel.hxx WindstilleLevel.cxx \
  StringConverter.hxx XMLhelper.hxx XMLhelper.cxx \
  LaserShoot.cxx   LaserShoot.hxx PlayerView.hxx PlayerView.cxx \
  BonusFlyer.hxx BonusFlyer.cxx GuileGameObj.hxx GuileGameObj.cxx \
  SpriteSmob.hxx SpriteSmob.cxx GuileGameObjFactory.hxx GuileGameObjFactory.cxx \
  TileMapSmob.hxx   TileMapSmob.cxx GameObjSmob.hxx GameObjSmob.cxx \
  GameWorldSmob.hxx GameWorldSmob.cxx globals.hxx SCMConverter.hxx

LDADD = editor/libturrican_editor.a


# EOF #