require "/home/ingo/.flexlay/supertux.rb"

BACKGROUND_LAYER  = 1
INTERACTIVE_LAYER = 2
FOREGROUND_LAYER  = 3

# bla
require "flexlay_wrap"
include Flexlay_wrap

require "flexlay.rb"
require "gameobj.rb"
require "sexpr.rb"

#import os
#import sys
#import code
#from flexlay import *
#from sexpr   import *

flexlay = Flexlay.new()
flexlay.init()

$editor = Editor.new()
$gui = $editor.get_gui_manager()

myrect      = CL_Rect.new(CL_Point.new(0, 56), CL_Size.new(665, 488+56))
$editor_map = EditorMapComponent.new(myrect, $gui.get_component())
$workspace  = Workspace.new(myrect.get_width(), myrect.get_height())
$editor_map.set_workspace($workspace)

# Tools
$tilemap_paint_tool  = TileMapPaintTool.new()
$tilemap_select_tool = TileMapSelectTool.new()
$zoom_tool           = ZoomTool.new()
$objmap_select_tool  = ObjMapSelectTool.new()

$workspace.set_tool($tilemap_paint_tool.to_tool());

$mysprite = make_sprite("../data/images/icons16/stock_paste-16.png")

$recent_files_menu = Menu.new(CL_Point.new(32*2, 54), $gui.get_component())

$recent_files.each do |filename|
  $recent_files_menu.add_item($mysprite, filename, proc{ supertux_load_level(filename) })
end

$minimap = Minimap.new($editor_map, CL_Rect.new(CL_Point.new(3, 488+3-14), 
                                               CL_Size.new(794-134-16, 50)), $editor_map)

require "gui.rb"

class Config
  attr_reader :datadir;

  def initialize()
    @datadir = "/home/ingo/cvs/supertux/supertux/data/"
    end
end

$datadir = "/home/ingo/cvs/supertux/supertux/data/"

class DisplayProperties
  attr_reader :layer, :show_all, :current_only
  attr_writer :layer, :show_all, :current_only

  def initialize()
    @layer        = INTERACTIVE_LAYER
    @show_all     = false
    @current_only = false
  end
    
  def set(map)

    if @current_only
      active   = CL_Color.new(255, 255, 255)
      deactive = CL_Color.new(0, 0, 0, 10)
    else
      active   = CL_Color.new(255, 255, 255)
      deactive = CL_Color.new(150, 150, 250, 150)
    end
    
    if (@show_all)
      map.foreground.set_foreground_color(active)
      map.interactive.set_foreground_color(active)
      map.background.set_foreground_color(active)
    else
      if (@layer == FOREGROUND_LAYER)
        map.foreground.set_foreground_color(active)
      else
        map.foreground.set_foreground_color(deactive)
      end
      
      if (@layer == INTERACTIVE_LAYER)
        map.interactive.set_foreground_color(active)
      else
        map.interactive.set_foreground_color(deactive)
      end
      
      if (@layer == BACKGROUND_LAYER)
        map.background.set_foreground_color(active)
      else
        map.background.set_foreground_color(deactive)
      end
    end
  end
end

# Load game tiles from filename into tileset
def Tileset_load(tileset, filename)
  tree = sexpr_read_from_file(filename)
  tree = tree[1..-1]
  for i in tree
    if i[0] == "tile"
      data  = i[1..-1]
      id    = get_value_from_tree(['id', '_'], data, -1)
      image = get_value_from_tree(['editor-images', '_'], data, false)
      
      if not(image)
        image = get_value_from_tree(['images', '_'], data, "notile.png")
      end
      
      if id != 0 # leave tile 0 transparent
        tileset.add_tile(id,
                         Tile.new($datadir + 'images/tilesets/' + image,
                                  CL_Color.new(255,   0,   0, 128)))
      end
    end
  end
end

$tileset = Tileset.new(32)
Tileset_load($tileset, $datadir + "images/tilesets/supertux.stgt")

$game_objects = [
  ["money", "images/shared/jumpy-left-middle-0.png", proc{BadGuy.new("money")}],
  ["snowball", "images/shared/snowball-left-0.png", proc{BadGuy.new("snowball")}],
  ["mriceblock", "images/shared/mriceblock-left-0.png", proc{BadGuy.new("mriceblock")}],
  ["mrbomb", "images/shared/mrbomb-left-0.png", proc{BadGuy.new("mrbomb")}],
  ["flame", "images/shared/flame-0.png", proc{BadGuy.new("flame")}], 
  ["stalactite", "images/shared/stalactite.png", proc{BadGuy.new("stalactite")}],
  ["fish", "images/shared/fish-left-0.png", proc{BadGuy.new("fish")}],
  ["flyingsnowball", "images/shared/flyingsnowball-left-0.png", proc{BadGuy.new("flyingsnowball")}],
  ["bouncingsnowball", "images/shared/bouncingsnowball-left-0.png", proc{BadGuy.new("bouncingsnowball")}],
  ["spiky", "images/shared/spiky-left-0.png", proc{BadGuy.new("spiky")}],
  ["playerspawn", "images/shared/resetpoint.png", proc{SpawnPoint.new()}],
  ["door", "images/shared/door.png", proc{Door.new()}],
  ["trampoline", "images/shared/trampoline-1.png", proc{BadGuy.new("trampoline")}]
]

require "level.rb"
require "sector.rb"
require "gui2.rb"

$gui.run()
flexlay.deinit()

# EOF #
