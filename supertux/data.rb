$game_objects = [
  ["money", "images/shared/jumpy-left-middle-0.png", "sprite",
    proc{|data| BadGuy.new("money")}],
  ["snowball", "images/shared/snowball-left-0.png", "sprite",
    proc{|data| BadGuy.new("snowball")}],
  ["mriceblock", "images/shared/mriceblock-left-0.png", "sprite",
    proc{|data| BadGuy.new("mriceblock")}],
  ["mrbomb", "images/shared/mrbomb-left-0.png", "sprite",
    proc{|data| BadGuy.new("mrbomb")}],
  ["flame", "images/shared/flame-0.png", "sprite",
    proc{|data| BadGuy.new("flame")}], 
  ["stalactite", "images/shared/stalactite.png", "sprite",
    proc{|data| BadGuy.new("stalactite")}],
  ["fish", "images/shared/fish-left-0.png", "sprite",
    proc{|data| BadGuy.new("fish")}],
  ["flyingsnowball", "images/shared/flyingsnowball-left-0.png", "sprite",
    proc{|data| BadGuy.new("flyingsnowball")}],
  ["bouncingsnowball", "images/shared/bouncingsnowball-left-0.png", "sprite",
    proc{|data| BadGuy.new("bouncingsnowball")}],
  ["spiky", "images/shared/spiky-left-0.png", "sprite",
    proc{|data| BadGuy.new("spiky")}],
  ["playerspawn", "images/shared/spawnpoint.png", "sprite",
    proc{|data| SpawnPoint.new(data)}],
  ["spawnpoint", "images/shared/spawnpoint.png", "sprite",
    proc{|data| SpawnPoint.new(data)}],
  ["door", "images/shared/door-1.png", "sprite",
    proc{|data| Door.new(data)}],
  ["trampoline", "images/shared/trampoline-1.png", "sprite",
    proc{|data| BadGuy.new("trampoline")}],
  ["secretarea", "images/shared/secretarea.png", "rect",
    proc{|data, sexpr| SecretArea.new(data, sexpr)}],
  ["sequencetrigger", "images/shared/sequencetrigger.png", "rect",
    proc{|data, sexpr| SequenceTrigger.new(data, sexpr)}]
]

def create_gameobject_from_data(objmap, name, sexpr)
  # Creates a gameobject from the given sexpr: "snowball", ((x 5) (y 5))
  
  object = $game_objects.find {|x| x[0] == name}
  if object != nil then
    (name, image, type, func) = object
    
    x = get_value_from_tree(["x", "_"], sexpr, 0)
    y = get_value_from_tree(["y", "_"], sexpr, 0)
    
    obj = create_gameobject(objmap, object, CL_Point.new(x, y), sexpr)
  else
    print "Error: Couldn't resolve object type: ", name, "\n"
    print "Sector: Unhandled tag: ", name, "\n"
  end
end

def create_gameobject(objmap, data, pos, sexpr = [])
  # Creates a gameobject the given position, data is the entry in the $game_objects table
  case data[2] 
    
  when "sprite" 
    obj = ObjMapSpriteObject.new(make_sprite($datadir + data[1]), pos, make_metadata(nil))
    obj.to_object.set_metadata(make_metadata(data[3].call(obj)))
    
  when "rect"
    obj = ObjMapRectObject.new(CL_Rect.new(CL_Point.new(pos.x.to_i, pos.y.to_i), CL_Size.new(64, 64)),
                               CL_Color.new(0, 0, 255, 128), make_metadata(nil))
    obj.to_object.set_metadata(make_metadata(data[3].call(obj, sexpr)))

  else
    raise "Error: Unknown object type droped: '#{data}'"
  end
  
  puts "Adding object to workspace: #{obj}"
  cmd = ObjectAddCommand.new(objmap)
  cmd.add_object(obj.to_object);
  $gui.workspace.get_map().execute(cmd.to_command());
  return obj
end

$solid_itiles = [10, 11, 12, 13, 14, 15, 20, 21, 22, 23, 30, 31, 113, 114]
$air_itiles   = [7, 8, 9, 16, 17, 18, 0]

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
