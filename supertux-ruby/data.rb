$game_objects = [
  ["angrystone", "images/creatures/angrystone/angrystone.sprite", :sprite,
    proc{|data, sexpr| BadGuy.new("angrystone")}],
  ["jumpy", "images/creatures/jumpy/left-middle.png", :sprite,
    proc{|data, sexpr| BadGuy.new("jumpy")}],
  ["snowball", "images/creatures/snowball/left-0.png", :sprite,
    proc{|data, sexpr| BadGuy.new("snowball")}],
  ["mriceblock", "images/creatures/mr_iceblock/left-0.png", :sprite,
    proc{|data, sexpr| BadGuy.new("mriceblock")}],
  ["mrbomb", "images/creatures/mr_bomb/left-0.png", :sprite,
    proc{|data, sexpr| BadGuy.new("mrbomb")}],
  ["flame", "images/creatures/flame/flame-0.png", :sprite,
    proc{|data, sexpr| BadGuy.new("flame")}], 
  ["stalactite", "images/creatures/stalactite/falling.png", :sprite,
    proc{|data, sexpr| BadGuy.new("stalactite")}],
  ["fish", "images/creatures/fish/left-0.png", :sprite,
    proc{|data, sexpr| BadGuy.new("fish")}],
  ["flyingsnowball", "images/creatures/flying_snowball/left-0.png", :sprite,
    proc{|data, sexpr| BadGuy.new("flyingsnowball")}],
  ["bouncingsnowball", "images/creatures/bouncing_snowball/left-0.png", :sprite,
    proc{|data, sexpr| BadGuy.new("bouncingsnowball")}],
  ["spiky", "images/creatures/spiky/left-0.png", :sprite,
    proc{|data, sexpr| BadGuy.new("spiky")}],
  ["mrtree", "images/creatures/mr_tree/walk-left-1.png", :sprite,
    proc{|data, sexpr| BadGuy.new("mrtree")}],
  ["poisonivy", "images/creatures/poison_ivy/left-0.png", :sprite,
    proc{|data, sexpr| BadGuy.new("poisonivy")}],
  ["zeekling", "images/creatures/zeekling/left-0.png", :sprite,
    proc{|data, sexpr| BadGuy.new("zeekling")}],
  ["kugelblitz", "images/creatures/kugelblitz/flying-0.png", :sprite,
    proc{|data, sexpr| BadGuy.new("kugelblitz")}],
  ["dispenser", "images/creatures/dispenser/working.png", :sprite,
    proc{|data, sexpr| Dispenser.new(data, sexpr)}],
  ["yeti", "images/creatures/yeti/yeti.png", :sprite,
    proc{|data, sexpr| BadGuy.new("yeti")}],
  ["stalactite_yeti", "images/engine/editor/stalactite_yeti.png", :sprite,
    proc{|data, sexpr| BadGuy.new("yeti_stalactite")}],
  ["spawnpoint", "images/engine/editor/spawnpoint.png", :sprite,
    proc{|data, sexpr| SpawnPoint.new(data, sexpr)}],
  ["ambient_sound", "images/engine/editor/ambientsound.png", :rect,
    proc{|data, sexpr| AmbientSound.new(data, sexpr)}],
  ["door", "images/objects/door/door-0.png", :sprite,
    proc{|data, sexpr| Door.new("door", data, sexpr)}],
#  ["hatch", "images/objects/hatch/hatch-0.png", :sprite,
#    proc{|data, sexpr| Door.new("hatch", data, sexpr)}],
  ["trampoline", "images/objects/trampoline/trampoline1-0.png", :sprite,
    proc{|data, sexpr| BadGuy.new("trampoline")}],
#  ["bell", "images/objects/bell/bell.sprite", :sprite,
#    proc{|data, sexpr| SimpleObject.new("bell")}],
  ["rock", "images/tiles/blocks/block11.png", :sprite,
    proc{|data, sexpr| SimpleObject.new("rock")}],
  ["unstable_tile", "images/objects/unstable_tile/crumbling-1.png", :sprite,
    proc{|data, sexpr| SimpleTileObject.new(data, "unstable_tile")}],
  ["infoblock", "images/engine/editor/infoblock.png", :sprite,
    proc{|data, sexpr| InfoBlock.new(data, sexpr)}],
  ["powerup", "images/engine/editor/powerup.png", :sprite,
    proc{|data, sexpr| Powerup.new(data, sexpr)}],
  ["secretarea", "images/engine/editor/secretarea.png", :rect,
    proc{|data, sexpr| SecretArea.new(data, sexpr)}],
  ["sequencetrigger", "images/engine/editor/sequencetrigger.png", :rect,
    proc{|data, sexpr| SequenceTrigger.new(data, sexpr)}],
  ["background", "images/engine/editor/background.png", :sprite,
    proc{|data, sexpr| Background.new(data, sexpr)}],
  ["gradient", "images/engine/editor/gradient.png", :sprite,
    proc{|data, sexpr| Gradient.new(data, sexpr)}],
  ["particles-snow", "images/engine/editor/snow.png", :sprite,
    proc{|data, sexpr| ParticleSystem.new("snow", sexpr)}],
  ["particles-clouds", "images/engine/editor/clouds.png", :sprite,
    proc{|data, sexpr| ParticleSystem.new("clouds", sexpr)}],
  ["particles-rain", "images/engine/editor/rain.png", :sprite,
    proc{|data, sexpr| ParticleSystem.new("rain", sexpr)}],
  ["leveltime", "images/engine/editor/clock.png", :sprite,
    proc{|data, sexpr| LevelTime.new(sexpr)}],
  ["point", "images/engine/editor/point.png", :sprite,
    proc{|data, sexpr| SimpleObject.new("point")}],
  ["platform", "images/objects/flying_platform/flying_platform-0.png", :sprite,
    proc{|data, sexpr| Platform.new(data, sexpr)}],
  ["scriptedobject", "images/engine/editor/scriptedobject.png", :sprite,
    proc{|data, sexpr| ScriptedObject.new(data, sexpr)}]
]

def create_gameobject_from_data(editormap, objmap, name, sexpr)
  # Creates a gameobject from the given sexpr: "snowball", ((x 5) (y 5))
  
  object = $game_objects.find {|x| x[0].to_sym == name}
  if object != nil then
    (name, image, type, func) = object
    
    x = get_value_from_tree(["x", "_"], sexpr, 0)
    y = get_value_from_tree(["y", "_"], sexpr, 0)
    
    create_gameobject(editormap, objmap, object, Pointf.new(x, y), sexpr)
  else
    print "Error: Couldn't resolve object type: ", name, "\n"
	print "Sector: Unhandled tag: ", name, "\n"
  end
end

def create_gameobject(editormap, objmap, data, pos, sexpr = [])
  (name, spritefile, type, func) = data
  
  # Creates a gameobject the given position, data is the entry in the $game_objects table
  case type
    when :sprite
      sprite = load_cl_sprite($datadir + spritefile)
      
      obj = ObjMapSpriteObject.new(sprite, pos, make_metadata(nil))
      gobj = func.call(obj, sexpr)
      obj.to_object.set_metadata(make_metadata(gobj))
      gobj.set_obj(obj)
    
    when :rect
      print "NewRect", pos.x, " -", pos.y, "\n"
      obj = ObjMapRectObject.new(Rect.new(Point.new(pos.x.to_i, pos.y.to_i), Size.new(64, 64)),
                                 Color.new(0, 0, 255, 128),
                                 make_metadata(nil))
      gobj = data[3].call(obj, sexpr)
      obj.to_object.set_metadata(make_metadata(gobj))
    else
      raise "Error: Unknown object type dropped: '#{data.inspect}'"
  end
  
  cmd = ObjectAddCommand.new(objmap)
  cmd.add_object(obj.to_object);
  editormap.execute(cmd.to_command());
  return obj
end

$solid_itiles = [10, 11, 12, 13, 14, 15, 20, 21, 22, 23, 30, 31, 113, 114]

$itile_conditions = [
  [0, 0, 0, 0, 0, 1, 0, 1, 1, 7],
  [0, 0, 1, 0, 0, 1, 0, 1, 1, 7],
  [0, 0, 0, 0, 0, 0, 0, 1, 1, 7],
  [0, 0, 0, 0, 0, 0, 1, 1, 1, 8],
  [0, 0, 0, 0, 0, 0, 1, 1, 0, 9],
  [0, 1, 1, 0, 0, 0, 0, 0, 0, 16],

  [1, 1, 1, 0, 0, 0, 0, 0, 0, 17],
  [1, 1, 1, 1, 0, 0, 0, 0, 0, 17],
  [1, 1, 1, 0, 0, 1, 0, 0, 0, 17],
  [1, 1, 1, 1, 0, 0, 1, 0, 0, 17],
  [1, 1, 1, 0, 0, 1, 0, 0, 1, 17],

  [1, 1, 0, 0, 0, 0, 0, 0, 0, 18],

  [0, 1, 1, 0, 1, 1, 0, 0, 0, 10],
  [1, 1, 1, 0, 1, 1, 0, 0, 0, 11],
  [1, 1, 0, 1, 1, 0, 0, 0, 0, 12],

  [0, 1, 1, 0, 1, 1, 0, 1, 1, 10],
  [1, 1, 1, 1, 1, 1, 1, 1, 1, 11],
  [1, 1, 0, 1, 1, 0, 1, 1, 0, 12],

  [0, 0, 0, 0, 1, 1, 0, 1, 1, 13],
  [0, 0, 0, 1, 1, 1, 1, 1, 1, 14],
  [0, 0, 0, 1, 1, 0, 1, 1, 0, 15],
  [1, 0, 0, 1, 1, 1, 1, 1, 1, 20],
  [1, 1, 0, 1, 1, 0, 1, 1, 1, 21],
  [0, 1, 1, 0, 1, 1, 1, 1, 1, 22],
  [0, 0, 1, 1, 1, 1, 1, 1, 1, 23],

  [1, 1, 1, 1, 1, 0, 1, 1, 0, 30],
  [1, 1, 1, 0, 1, 1, 0, 1, 1, 31],

  [0, 0, 0, 1, 1, 0, 1, 1, 1, 113],
  [0, 0, 0, 0, 1, 1, 1, 1, 1, 114],

]

# EOF #
