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
  ["playerspawn", "images/shared/resetpoint.png", "sprite",
    proc{|data| SpawnPoint.new(data)}],
  ["door", "images/shared/door-1.png", "sprite",
    proc{|data| Door.new(data)}],
  ["trampoline", "images/shared/trampoline-1.png", "sprite",
    proc{|data| BadGuy.new("trampoline")}],
  ["secretarea", "images/shared/secretarea.png", "rect",
    proc{|data| SecretArea.new(data)}]
]

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
