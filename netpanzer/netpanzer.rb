##  $Id$
## 
##  Flexlay - A Generic 2D Game Editor
##  Copyright (C) 2002 Ingo Ruhnke <grumbel@gmx.de>
##
##  This program is free software; you can redistribute it and/or
##  modify it under the terms of the GNU General Public License
##  as published by the Free Software Foundation; either version 2
##  of the License, or (at your option) any later version.
##
##  This program is distributed in the hope that it will be useful,
##  but WITHOUT ANY WARRANTY; without even the implied warranty of
##  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
##  GNU General Public License for more details.
## 
##  You should have received a copy of the GNU General Public License
##  along with this program; if not, write to the Free Software
##  Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.

require "flexlay_wrap"
include Flexlay_wrap

require "flexlay.rb"
require "netpanzer_wrap"
include Netpanzer_wrap

require "netpanzerbrushes.rb"

class Config
  attr_accessor :datadir, :recent_files

  def initialize()
    @datadir      = "./"
    @recent_files = []
  end
end

class Level
  attr_accessor :filename, :data, :editormap, :objects

  def initialize(*params)
    if params.length == 2 then
      (width, height) = params
      @data = NetPanzerFileStruct.new($tileset, width, height)

    elsif len(params) == 1 then
      (@filename,) = params
      @data = NetPanzerFileStruct.new($tileset, @filename)
    end      

    @objects   = ObjectLayer.new()
    @editormap = EditorMap.new()
    @editormap.add_layer(@data.get_tilemap().to_layer())
    @editormap.add_layer(@objects.to_layer())

    # FIXME: Data might not get freed since its 'recursively' refcounted
    @editormap.set_metadata(make_metadata(self))
  end

  def save_optfile(filename)
    outpots = [] # FIXME
    
    f = open(filename, "w")
    f.write("ObjectiveCount: %d\n\n" % len(outposts))
    for (name, x , y) in outpots:
        f.write("Name: %s\n" % "Foobar")
      f.write("Location: %d %d\n\n" % x.to_i/32, y.to_i/32)
    end
  end

  def save_spnfile(filename)
    spawnpoints = []
    f = open(filename, "w")

    f.write("SpawnCount: %d\n\n" % len(spawnpoints))
    spawnpoints.each {|x, y|
      f.print("Location: %d %d\n" % [x.to_i/32, y.to_i/32])
    }
  end

  def save(filename)
    if filename[-4..-1] == ".npm":
        data.save(filename)
      save_optfile(filename[0..-4] + ".opt")
      save_optfile(filename[0..-4] + ".spn")
    else
      raise "Fileextension not valid, must be .npm!"
    end
  end

  def activate(workspace)
    $workspace.set_map(@editormap)
    TilemapLayer.set_current(@data.get_tilemap())
    ObjectLayer.set_current(@objects)
  end
end

flexlay = Flexlay.new()
flexlay.init()

$config = Config.new()

$tileset = Tileset.new(32)
load_netpanzer_tiles($tileset)

$editor = Editor.new()
$gui = $editor.get_gui_manager()

myrect = CL_Rect.new(CL_Point.new(0, 56), CL_Size.new(665, 488+56))
$editor_map = EditorMapComponent.new(myrect, $gui.get_component())
$workspace  = Workspace.new(myrect.get_width(), myrect.get_height())
$editor_map.set_workspace($workspace)

option_panel = Panel.new(CL_Rect.new(CL_Point.new(666, 56), CL_Size.new(134, 488+56)), $gui.get_component())

brushbox = CL_ListBox.new(CL_Rect.new(CL_Point.new(3, 3), CL_Size.new(128, 488+56-128-9)), option_panel)

$brushes.each {|i|
  (index, width, height, name) = i
  brushbox.insert_item("%s - %sx%s" % [name, width, height])
}

def brushbox_change(index)
  (start, width,  height, name) = $brushes[index]
  brush = TileBrush.new(width, height)
  brush.set_data(Range.new(start, start + width*height).to_a)
  $tilemap_paint_tool.set_brush(brush)
end

connect_v1(brushbox.sig_highlighted(), method(:brushbox_change))

# Tools
$tilemap_paint_tool  = TileMapPaintTool.new()
$tilemap_select_tool = TileMapSelectTool.new()
$zoom_tool           = ZoomTool.new()
$objmap_select_tool  = ObjMapSelectTool.new()

$workspace.set_tool($tilemap_paint_tool.to_tool());

def on_map_change()
# FIXME:
#   if ($workspace.get_map().undo_stack_size() > 0)
#     undo_icon.enable()
#   else
#     undo_icon.disable()
#   end

#   if ($workspace.get_map().redo_stack_size() > 0)
#     redo_icon.enable()
#   else
#     redo_icon.disable()        
#   end
end

startlevel = Level.new(256, 256)
startlevel.activate($workspace)
connect(startlevel.editormap.sig_change(), on_map_change)

button_panel = Panel.new(CL_Rect.new(CL_Point.new(0, 23), CL_Size.new(800, 33)), $gui.get_component())

def gui_level_save_as()
  $save_dialog.set_filename(os.path.dirname($save_dialog.get_filename()) + "/")
  $save_dialog.run(netpanzer_save_level)
end

def gui_level_save()
  if $workspace.get_map().get_metadata().filename:
      $save_dialog.set_filename($workspace.get_map().get_metadata().filename)
  else
    $save_dialog.set_filename(os.path.dirname($save_dialog.get_filename())  + "/")
  end
  
  $save_dialog.run(netpanzer_save_level)
end

def gui_level_load()
  $load_dialog.run(netpanzer_load_level)
end

# new_icon         = Icon.new(CL_Rect.new(CL_Point.new(p.inc(0),  2), CL_Size.new(32, 32)),
#                         make_sprite("../data/images/icons24/stock_new.png"), "Some tooltip", button_panel);
# load_icon        = Icon(CL_Rect(CL_Point(p.inc(32), 2), CL_Size(32, 32)),
#                         make_sprite("../data/images/icons24/stock_open.png"), "Some tooltip", button_panel);
# load_recent_icon = Icon(CL_Rect(CL_Point(p.inc(32), 2), CL_Size(16, 32)),
#                         make_sprite("../data/images/icons24/downarrow.png"), "Some tooltip", button_panel);
# save_icon        = Icon(CL_Rect(CL_Point(p.inc(16), 2), CL_Size(32, 32)),
#                         make_sprite("../data/images/icons24/stock_save.png"), "Some tooltip", button_panel);
# save_as_icon     = Icon(CL_Rect(CL_Point(p.inc(32), 2), CL_Size(32, 32)),
#                         make_sprite("../data/images/icons24/stock_save_as.png"), "Some tooltip", button_panel);

# load_icon.set_callback(gui_level_load)
# load_recent_icon.set_callback(lambda: recent_files_menu.run())
# save_icon.set_callback(gui_level_save)
# save_as_icon.set_callback(gui_level_save_as)

# copy_icon    = Icon.new(CL_Rect(CL_Point(p.inc(48), 2), CL_Size(32, 32)),
#                     make_sprite("../data/images/icons24/stock_copy.png"), "Some tooltip", button_panel);
# paste_icon   = Icon.new(CL_Rect(CL_Point(p.inc(32), 2), CL_Size(32, 32)),
#                     make_sprite("../data/images/icons24/stock_paste.png"), "Some tooltip", button_panel);

# undo_icon = Icon.new(CL_Rect.new(CL_Point.new(p.inc(48), 2), CL_Size(32, 32)),
#                  make_sprite("../data/images/icons24/stock_undo.png"), "Some tooltip", button_panel);
# redo_icon = Icon.new(CL_Rect.new(CL_Point.new(p.inc(32), 2), CL_Size(32, 32)),
#                  make_sprite("../data/images/icons24/stock_redo.png"), "Some tooltip", button_panel);

# undo_icon.set_callback(lambda: $workspace.get_map().undo())
# redo_icon.set_callback(lambda: $workspace.get_map().redo())

# undo_icon.disable()
# redo_icon.disable()

def gui_toggle_grid()
  tilemap = $workspace.get_map().get_metadata().data.get_tilemap()

  #FIXMEgrid_status = not(tilemap.get_draw_grid())
  #tilemap.set_draw_grid(grid_status)

  if tilemap.get_draw_grid() then
    grid_icon.set_down()
  else
    grid_icon.set_up()
    
    grid_icon = Icon(CL_Rect(CL_Point(p.inc(48), 2), CL_Size(32, 32)),
                     make_sprite("../data/images/icons24/grid.png"), "Some tooltip", button_panel);
    grid_icon.set_callback(proc{gui_toggle_grid})

    layer_menu = Menu(CL_Point(32*11+2, 54), $gui.get_component())
  end
end


def set_tilemap_paint_tool()
  $workspace.set_tool($tilemap_paint_tool.to_tool())
#   $paint.set_down()
#   $select.set_up()
#   $zoom.set_up()
#   $object.set_up()
end

def set_tilemap_select_tool()
  $workspace.set_tool($tilemap_select_tool.to_tool())
#   $paint.set_up()
#   $select.set_down()
#   $zoom.set_up()
#   $object.set_up()
end

def set_zoom_tool()
  $workspace.set_tool($zoom_tool.to_tool())
#   $paint.set_up()
#   $select.set_up()
#   $zoom.set_down()
#   $object.set_up()
end

def set_objmap_select_tool()
  $workspace.set_tool($objmap_select_tool.to_tool())
#   $paint.set_up()
#   $select.set_up()
#   $zoom.set_up()
#   $object.set_down()
end

toolbar = Panel.new(CL_Rect.new(CL_Point.new(0, 23+33), CL_Size.new(33, 32*4+2)), $gui.get_component())

paint = Icon.new(CL_Rect.new(CL_Point.new(2, 32*0+2), CL_Size.new(32, 32)), make_sprite("../data/images/tools/stock-tool-pencil-22.png"), "Some tooltip", toolbar);
paint.set_callback(method(:set_tilemap_paint_tool))

select = Icon.new(CL_Rect.new(CL_Point.new(2, 32*1+2), CL_Size.new(32,32)), make_sprite("../data/images/tools/stock-tool-rect-select-22.png"), "Some tooltip", toolbar);
select.set_callback(method(:set_tilemap_select_tool))

zoom = Icon.new(CL_Rect.new(CL_Point.new(2, 32*2+2), CL_Size.new(32,32)), make_sprite("../data/images/tools/stock-tool-zoom-22.png"), "Some tooltip", toolbar);
zoom.set_callback(method(:set_zoom_tool))

object = Icon.new(CL_Rect.new(CL_Point.new(2, 32*3+2), CL_Size.new(32,32)), make_sprite("../data/images/tools/stock-tool-clone-22.png"), "Some tooltip", toolbar);
object.set_callback(method(:set_objmap_select_tool))

level = nil

mysprite = make_sprite("../data/images/icons16/stock_paste-16.png")

def netpanzer_load_level(filename)
  level = Level(filename)
  level.activate(workspace)
  connect(level.editormap.sig_change(), on_map_change)
  
  if not(has_element(config.recent_files, filename))
    config.recent_files.append(filename)
    recent_files_menu.add_item(mysprite, filename, proc{ netpanzer_load_level(filename) })

    minimap.update_minimap()
  end
end

def netpanzer_save_level(filename)
  $workspace.get_map().get_metadata().save(filename)
end

recent_files_menu = Menu.new(CL_Point.new(32*2, 54), $gui.get_component())
for filename in $config.recent_files
    recent_files_menu.add_item(mysprite, filename, proc{ netpanzer_load_level(filename) })
end

def has_element(lst, el)
  lst.each {|i|
    if i == el then 
      return True
    end
  }
  return False
end

menu = CL_Menu.new($gui.get_component())
menu.add_item("File/Open...", proc{gui_level_load})
menu.add_item("File/Save...", proc{gui_level_save})
menu.add_item("File/Save As...", proc{gui_level_save_as})
menu.add_item("File/Quit",  proc{$gui.quit})

def gui_set_zoom(zoom)
  gc = $editor_map.get_workspace().get_gc_state()
  pos = gc.get_pos()
  gc.set_zoom(zoom)
  gc.set_pos(pos)
end

menu.add_item("Zoom/1:4 (25%) ",  proc{ gui_set_zoom(0.25) })
menu.add_item("Zoom/1:2 (50%) ",  proc{ gui_set_zoom(0.5) })
menu.add_item("Zoom/1:1 (100%) ", proc{ gui_set_zoom(1.0) }) 
menu.add_item("Zoom/2:1 (200%) ", proc{ gui_set_zoom(2.0) })
menu.add_item("Zoom/4:1 (400%) ", proc{ gui_set_zoom(4.0) })

# minimap_panel = Panel(CL_Rect(CL_Point(0, 600-56), CL_Size(800-134, 56)), $gui.get_component())
minimap = Minimap.new($editor_map, CL_Rect.new(CL_Point.new(3, 488+56 - 128-3), CL_Size.new(128, 128)), option_panel)

$load_dialog = SimpleFileDialog.new("Load netPanzer Level", "Load", "Cancel", $gui.get_component())
$load_dialog.set_filename($config.datadir + "maps/")
$save_dialog = SimpleFileDialog.new("Save netPanzer Level as...", "Save", "Cancel", $gui.get_component())
$save_dialog.set_filename($config.datadir + "maps/")

set_tilemap_paint_tool()

$gui.run()

flexlay.deinit()
print "deinit done"

# EOF #
