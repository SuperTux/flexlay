BACKGROUND_LAYER  = 1
INTERACTIVE_LAYER = 2
FOREGROUND_LAYER  = 3

# bla
require "flexlay_wrap"
include Flexlay_wrap

require "flexlay.rb"

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

myrect     = CL_Rect.new(CL_Point.new(0, 56), CL_Size.new(665, 488+56))
$editor_map = EditorMapComponent.new(myrect, $gui.get_component())
$workspace  = Workspace.new(myrect.get_width(), myrect.get_height())
$editor_map.set_workspace($workspace)

# Tools
$tilemap_paint_tool  = TileMapPaintTool.new()
$tilemap_select_tool = TileMapSelectTool.new()
$zoom_tool           = ZoomTool.new()
$objmap_select_tool  = ObjMapSelectTool.new()

$workspace.set_tool($tilemap_paint_tool.to_tool());

$recent_files = []
$recent_files_menu = Menu.new(CL_Point.new(32*2, 54), $gui.get_component())
for filename in $recent_files
  $recent_files_menu.add_item(mysprite, filename, proc{ supertux_load_level(filename) })
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

  layer    = INTERACTIVE_LAYER
  show_all = false
  current_only = false
    
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

def Tileset_load(tileset, filename)
  "Load game tiles from filename into tileset"
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

$game_objects = [["money", "images/shared/jumpy-left-middle-0.png"],
                 ["snowball", "images/shared/snowball-left-0.png"],
                 ["mriceblock", "images/shared/mriceblock-left-0.png"],
                 ["mrbomb", "images/shared/mrbomb-left-0.png"],
                 ["flame", "images/shared/flame-0.png"], 
                 ["stalactite", "images/shared/stalactite.png"],
                 ["fish", "images/shared/fish-left-0.png"],
                 ["flyingsnowball", "images/shared/flyingsnowball-left-0.png"],
                 ["bouncingsnowball", "images/shared/bouncingsnowball-left-0.png"],
                 ["spiky", "images/shared/spiky-left-0.png"],
                 ["resetpoint", "images/shared/resetpoint.png"],
                 ["playerspawn", "images/shared/resetpoint.png"],
                 ["door", "images/shared/door.png"],
                 ["trampoline", "images/shared/trampoline-1.png"]]


class Level
  version = 2
  filename = nil
  
  name   = "no name"
  author = "no author"
  theme = "antarctica"
  time = 999
  music = "Mortimers_chipdisko.mod"
  
  objects = nil
  camera  = nil
  
  sectors = nil
  current_sector = nil

  attr_reader :version, :filename, :name, :author, :theme, :time, :music, :objects, :camera, :sectors, :current_sector
  attr_writer :version, :filename, :name, :author, :theme, :time, :music, :objects, :camera, :sectors, :current_sector
  
  def initialize(*params)
    if params.length() == 2 then
      # New Level
      (width, height) = params
      
      @name   = "No Name"
      @author = "No Author"
      
      @width  = width
      @height = height
      
      @current_sector = Sector.new(self)
      @current_sector.new_from_size(width, height)
      @sectors = []
      @sectors.push(@current_sector)
      
    elsif params.length() == 1 then
      # Load Level from file
      (@filename,) = params
      
      tree = sexpr_read_from_file(@filename)
      if tree == nil
        raise("Couldn't load level: ", filename)
      end
      
      data = tree[1..-1]
      
      @version = get_value_from_tree(["version", "_"], data, 1)
      
      if (@version == 1) then
        parse_v1(data)
      else
        parse_v2(data)
      end
    else
      raise "Wrong arguments for SuperTux::___init__"
    end
  end
  
  def parse_v2(data)
    @name    = get_value_from_tree(["name", "_"], data, "no name")
    @author  = get_value_from_tree(["author", "_"], data, "no author")
    @time    = int(get_value_from_tree(["time", "_"], data, "999"))
    
    @sectors = []
    for sec in sexpr_filter("sector", data)
      sector = Sector.new(self)
      sector.load_v2(sec)
      @sectors.append(sector)
      if sector.name == "main"
        @current_sector = sector
      end
    end
    
    if @current_sector == nil
      print "Error: No main sector defined: ", sectors
    end
  end

  def parse_v1(data)
    sector = Sector.new(self)
    sector.load_v1(data)
    
    @sectors = []
    @sectors.append(sector)
    @current_sector = sector
    
    @name    = get_value_from_tree(["name", "_"], data, "no name")
    @author  = get_value_from_tree(["author", "_"], data, "no author")
    @time    = int(get_value_from_tree(["time", "_"], data, "999"))       
  end
  
  def save(filename)
    save_v2(filename)
  end
  
  def save_v2(filename)
    f = File.new(filename, "w")
    f.write(";; Generated by Flexlay Editor\n" +
                                                "(supertux-level\n")
    f.write("  (version 2)\n")
    f.write("  (name   \"%s\")\n" % @name)
    f.write("  (author \"%s\")\n" % @author)
    f.write("  (width  %s)\n"  % @width)
    f.write("  (height  %s)\n" % @height)
    
    f.write("  (music  \"%s\")\n" % @music)
    f.write("  (time   \"%s\")\n" % @time)
    
    f.write("  (gravity %d)\n" % @gravity)
    
    f.write("  (theme \"%s\")\n" % @theme)
    
    f.write("  (interactive-tm\n")
    for i in interactive.get_data()
      f.write("%d " % i)
    end
    f.write("  )\n\n")
    
    f.write("  (background-tm\n")
    for i in background.get_data()
      f.write("%d " % i)
    end
    f.write("  )\n\n")

    f.write("  (foreground-tm\n")
    for i in foreground.get_data()
      f.write("%d " % i)
    end
    f.write("  )\n\n")

    f.write("  (camera\n")
    f.write("    (mode \"autoscroll\")\n")
    f.write("    (path\n")
    for obj in objects.get_objects()
      pathnode = get_python_object(obj.get_metadata())
      if (pathnode.__class__ == PathNode)
        f.write("     (point (x %d) (y %d) (speed 1))\n" % obj.get_pos().x, obj.get_pos().y)
      end
    end
    f.write("  ))\n\n")

    f.write("  (objects\n")
    for obj in @objects.get_objects()
      badguy = get_python_object(obj.get_metadata())
      if (badguy.__class__ == BadGuy)
        pos    = obj.get_pos()
        if (badguy.type != "resetpoint")
          f.write("     (%s (x %d) (y %d))\n" % badguy.type, int(pos.x), int(pos.y))
        end
      end
    end
    f.write("  )\n\n")

    f.write("  (reset-points\n")
    for obj in @objects.get_objects()
      badguy = get_python_object(obj.get_metadata())
      if (badguy.__class__ == BadGuy)
        pos    = obj.get_pos()
        if (badguy.type == "resetpoint")
          f.write("     (point (x %d) (y %d))\n" % int(pos.x), int(pos.y))
        end
      end
    end
    f.write("  )\n\n")
    
    f.write(" )\n\n;; EOF ;;\n")
  end
  
  def save_v1(filename)
    f = File.new(filename, "w")
    f.write(";; Generated by Flexlay Editor\n" +
                                                "(supertux-level\n")
    f.write("  (version 1)\n")
    f.write("  (name   \"%s\")\n" % @name)
    f.write("  (author \"%s\")\n" % @author)
    f.write("  (width  %s)\n"  % @width)
    f.write("  (height  %s)\n" % @height)
    
    f.write("  (music  \"%s\")\n" % @music)
    f.write("  (time   \"%s\")\n" % @time)
    
    f.write("  (gravity %d)\n" % @gravity)
    
    f.write("  (theme \"%s\")\n" % @theme)
    
    f.write("  (interactive-tm\n")
    for i in @interactive.get_data()
      f.write("%d " % i)
    end
    f.write("  )\n\n")

    f.write("  (background-tm\n")
    for i in @background.get_data()
      f.write("%d " % i)
    end
    f.write("  )\n\n")

    f.write("  (foreground-tm\n")
    for i in @foreground.get_data()
      f.write("%d " % i)
    end
    f.write("  )\n\n")

    f.write("  (camera\n")
    f.write("    (mode \"autoscroll\")\n")
    f.write("    (path\n")
    for obj in @objects.get_objects()
      pathnode = get_python_object(obj.get_metadata())
      if (pathnode.__class__ == PathNode)
        f.write("     (point (x %d) (y %d) (speed 1))\n" % obj.get_pos().x, obj.get_pos().y)
      end
    end
    f.write("  ))\n\n")
    
    f.write("  (objects\n")
    for obj in @objects.get_objects()
      badguy = get_python_object(obj.get_metadata())
      if (badguy.__class__ == BadGuy)
        pos    = obj.get_pos()
        if (badguy.type != "resetpoint")
          f.write("     (%s (x %d) (y %d))\n" % badguy.type, int(pos.x), int(pos.y))
        end
      end
    end
    f.write("  )\n\n")
    
    f.write("  (reset-points\n")
    for obj in @objects.get_objects()
      badguy = get_python_object(obj.get_metadata())
      if (badguy.__class__ == BadGuy)
        pos    = obj.get_pos()
        if (badguy.type == "resetpoint")
          f.write("     (point (x %d) (y %d))\n" % (pos.x.to_i), pos.y.to_i)
        end
      end
    end
    f.write("  )\n\n")
    
    f.write(" )\n\n;; EOF ;;\n")
  end

  def activate_sector(sector, workspace)
    for sec in @sectors
      if sec.name == sector
        sec.activate(workspace)
        break
      end
    end
  end

  def add_sector(sector)
    @sectors.push(sector)
  end

  def get_sectors()
    return @sectors.map {|sec| sec.name}
  end

  def activate(workspace)
    @current_sector.activate(workspace)
  end
end

class Sector
  parent    = nil
  name      = nil
  song      = nil
  gravity   = 10.0
  
  width  = nil
  height = nil
  
  background  = nil
  interactive = nil
  foreground  = nil
  
  objects   = nil
  editormap = nil

  attr_reader :name, :background, :interactive, :foreground, :parent, :width, :height
  attr_writer :name, :song, :gravity
 
  def initialize(parent)
    @parent = parent
  end

  def get_level()
    return @parent
  end

  def resize(size, pos)
    @width  = size.width
    @height = size.height
    @background.resize(size, pos)
    @interactive.resize(size, pos)
    @foreground.resize(size, pos)
  end

  def new_from_size(width, height)
    @name = "<No Name>"
    @song = "<No Song>"
    @gravity = 10.0
    
    @width  = width
    @height = height
    
    @foreground  = TilemapLayer.new($tileset, @width, @height)
    @interactive = TilemapLayer.new($tileset, @width, @height)
    @background  = TilemapLayer.new($tileset, @width, @height)       
    @objects = ObjectLayer.new()

    @editormap = EditorMap.new()
    @editormap.add_layer(@background.to_layer())
    @editormap.add_layer(@interactive.to_layer())
    @editormap.add_layer(@objects.to_layer())
    @editormap.add_layer(@foreground.to_layer())
    # FIXME: Data might not get freed since its 'recursively' refcounted
    @editormap.set_metadata(make_metadata(self))
    return self
  end

  def load_v1(data)
    @name = "<No Name>"
    @song = "<No Song>"
    @gravity = 10.0
    
    @width  = get_value_from_tree(["width", "_"], data, 20)
    @height = get_value_from_tree(["height""_"], data, 15)
    
    @foreground  = TilemapLayer($tileset, @width, @height)
    @foreground.set_data(get_value_from_tree(["foreground-tm"], data, []))
    
    @interactive = TilemapLayer($tileset, @width, @height)
    @interactive.set_data(get_value_from_tree(["interactive-tm"], data, []))
    
    @background  = TilemapLayer($tileset, @width, @height)
    @background.set_data(get_value_from_tree(["background-tm"], data, []))
    
    @objects = ObjectLayer.new()
    for i in get_value_from_tree(["objects"], data, [])
      type = i[0]
      x = get_value_from_tree(["x", "_"], i[1..-1], [])
      y = get_value_from_tree(["y", "_"], i[1..-1], [])
      object = $game_objects.find{|x| x[0] == type}
      if object != nil
        @objects.add_object(ObjMapSpriteObject(make_sprite($datadir + object[1]),
                                               CL_Point(x, y),
                                               make_metadata(BadGuy(object[0]))).to_object())
      else
        print "Error: Couldn't resolve object type: ", type
      end
    end
    
    for i in get_value_from_tree(["reset-points"], data, [])
      type = i[0]
      x = get_value_from_tree(["x", "_"], i[1..-1], [])
      y = get_value_from_tree(["y", "_"], i[1..-1], [])
      object = find($game_objects, "resetpoint")
      @objects.add_object(ObjMapSpriteObject(make_sprite($datadir + object[1]),
                                             CL_Point(x, y),
                                             make_metadata(BadGuy(object[0]))).to_object())
    end

    @editormap = EditorMap.new()
    @editormap.add_layer(@background.to_layer())
    @editormap.add_layer(@interactive.to_layer())
    @editormap.add_layer(@objects.to_layer())
    @editormap.add_layer(@foreground.to_layer())
    # FIXME: Data might not get freed since its 'recursively' refcounted
    @editormap.set_metadata(make_metadata(self))
  end
  
  def load_v2(data)
    @name = "<No Name>"
    @song = "<No Song>"
    @gravity = 10.0
    
    @objects = ObjectLayer.new()
    for i in data
      (name,data) = i[0], i[1..-1]
      if name == "name"
        @name = data[0]
      elsif name == "gravity"
        @gravity = int(data[0])
      elsif name == "playerspawn"
        print "playerspawn unhandled"
      elsif name == "tilemap"
        width   = get_value_from_tree(["width", "_"], data, 20)
        height  = get_value_from_tree(["height", "_"], data, 15)
        solid   = get_value_from_tree(["solid", "_"], data, false)

        tilemap = TilemapLayer($tileset, width, height)
        tilemap.set_data(get_value_from_tree(["tiles"], data, []))
        
        print "Solid: ", solid
        if solid and @interactive == nil
          @interactive = tilemap
          @width       = width
          @height      = height
        elsif @background == nil
          @background = tilemap
        elsif @foreground == nil
          @foreground = tilemap
        else
          print "Error: Duplicate tilemap in levelfile"
        end
      elsif name == "background"
        print "background unhandled"
      else
        object = $game_objects.find{|x| x[0] == name}
        if object != nil
          (name, image) = object
          x = get_value_from_tree(["x", "_"], data, [])
          y = get_value_from_tree(["y", "_"], data, [])
          @objects.add_object(ObjMapSpriteObject.new(make_sprite($datadir + image),
                                                     CL_Point.new(x, y),
                                                     make_metadata(BadGuy.new(name))).to_object())
        else
          print "Error: Couldn't resolve object type: ", name
          print "Sector: Unhandled tag: ", name
        end
      end
    end
    
    
    if (@background == nil)
      @background = TilemapLayer.new($tileset, width, height)
    end

    if (@interactive == nil)
      @interactive = TilemapLayer.new($tileset, width, height)
    end
    
    if (@foreground == nil)
      @foreground = TilemapLayer.new($tileset, width, height)
    end

    @editormap = EditorMap()
    @editormap.add_layer(@background.to_layer())
    @editormap.add_layer(@interactive.to_layer())
    @editormap.add_layer(@foreground.to_layer())
    @editormap.add_layer(@objects.to_layer())
    
    @editormap.set_metadata(make_metadata(self))
  end

  def activate(workspace)
    workspace.set_map(@editormap)
    TilemapLayer.set_current(@interactive)
    ObjectLayer.set_current(@objects)
    connect(@editormap.sig_change(), proc{on_map_change()})
  end
end

require "gui2.rb"

$gui.run()
flexlay.deinit()

# EOF #