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
require "level.rb"
require "gameobjects.rb"

$screen  = CL_Size.new(1024, 768)

flexlay = Flexlay.new()
flexlay.init($screen.width, $screen.height)

class Config
  attr_accessor :datadir, :recent_files

  def initialize()
    @datadir      = "./"
    @recent_files = []
  end
end

$config = Config.new()

$datadir = "/home/ingo/games/netpanzer-0.1.5"

$brushes.each_with_index{|(start, width, height, name), index|
  NetPanzerData::instance().register_tilegroup(start, width, height)
}

NetPanzerData::instance().load_data($datadir)

$tileset = NetPanzerData::instance().get_tileset()

$editor = Editor.new()
$gui = $editor.get_gui_manager()

myrect = CL_Rect.new(CL_Point.new(0, 56), CL_Size.new($screen.width-134-1, ($screen.height-112)+56))
$editor_map = EditorMapComponent.new(myrect, $gui.get_component())
$workspace  = Workspace.new(myrect.get_width(), myrect.get_height())
$editor_map.set_workspace($workspace)

$option_panel = Panel.new(CL_Rect.new(CL_Point.new($screen.width-134, 56), CL_Size.new(134, $screen.height-112+56)), $gui.get_component())

$brushbox = CL_ListBox.new(CL_Rect.new(CL_Point.new(3, 3), CL_Size.new(128, $screen.height-112+56-128-9)), $option_panel)
$brushbox.show(false)

$objectselector = ObjectSelector.new(CL_Rect.new(CL_Point.new(3, 3), CL_Size.new(128, $screen.height-112+56-128-9)),
                                     64, 64, $option_panel)

$resources = CL_ResourceManager.new("netpanzersprites.xml")

$objectselector.add_brush(ObjectBrush.new(GameObjects::Outpost.get_sprite(),
                                          make_metadata(proc{GameObjects::Outpost.new()})))
$objectselector.add_brush(ObjectBrush.new(GameObjects::SpawnPoint.get_sprite(),
                                          make_metadata(proc{GameObjects::SpawnPoint.new()})))

$brushes.size.times {|i|
  $objectselector.add_brush(ObjectBrush.new(make_sprite("thumbnails/#{i}.png"),
                                            make_metadata(proc{GameObjects::TileObject.new(i)})))
}

$objectselector.show(true)

def on_object_drop(brush, pos)
  obj = get_ruby_object(brush.get_data()).call()
  pos = $editor_map.screen2world(pos)
  sprite_obj = ObjMapSpriteObject.new(obj.get_sprite(), pos, make_metadata(obj))
  obj.data = sprite_obj
  
  cmd = ObjectAddCommand.new($workspace.get_map().get_data().objects)
  cmd.add_object(sprite_obj.to_object)
  $workspace.get_map().execute(cmd.to_command())
end

connect_v2_ObjectBrush_Point($objectselector.sig_drop(), method(:on_object_drop))

$brushes.each {|i|
  (index, width, height, name) = i
  $brushbox.insert_item("%s - %sx%s" % [name, width, height])
}

def brushbox_change(index)
  (start, width,  height, name) = $brushes[index]
  brush = TileBrush.new(width, height)
  brush.set_data(Range.new(start, start + width*height).to_a)
  $tilemap_paint_tool.set_brush(brush)
end

connect_v1($brushbox.sig_highlighted(), method(:brushbox_change))

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

# $startlevel = Level.new(256, 256)
# $startlevel.activate($workspace)
# connect($startlevel.editormap.sig_change(), proc{on_map_change})

def gui_level_save_as()
  $save_dialog.set_filename(File::dirname($save_dialog.get_filename()) + "/")
  $save_dialog.run(method(:netpanzer_save_level))
end

def gui_level_new()
  dialog = GenericDialog.new("SecretArea Property Dialog", $gui.get_component())
  dialog.add_string("Name: ", "New Level")
  dialog.add_int("Width: ", 128)
  dialog.add_int("Height: ", 128)
  dialog.set_callback(proc{|name, width, height|
                        level = Level.new(width, height)
                        level.activate($workspace)
                        level.name = name
                        connect(level.editormap.sig_change(), proc{on_map_change})
                      })
end

def gui_level_save()
  if $workspace.get_map().get_metadata().filename:
      $save_dialog.set_filename($workspace.get_map().get_metadata().filename)
  else
    $save_dialog.set_filename(File::dirname($save_dialog.get_filename())  + "/")
  end
  
  $save_dialog.run(method(:netpanzer_save_level))
end

def gui_level_load()
  $load_dialog.run(method(:netpanzer_load_level))
end

$button_panel = ButtonPanel.new(0, 23, $screen.width, 33, true, $gui.get_component)

$button_panel.add_icon("../data/images/icons24/stock_new.png",
                       proc{ gui_level_new() })
$button_panel.add_icon("../data/images/icons24/stock_open.png", 
                       proc{ gui_level_load() })
$button_panel.add_small_icon("../data/images/icons24/downarrow.png", 
                             proc{ gui_level_load() })
$button_panel.add_icon("../data/images/icons24/stock_save.png",
                       proc{ gui_level_save() })
$button_panel.add_icon("../data/images/icons24/stock_save_as.png", 
                       proc{ gui_level_save_as() })

$button_panel.add_seperator()
$button_panel.add_icon("../data/images/icons24/stock_copy.png", proc{})
$button_panel.add_icon("../data/images/icons24/stock_paste.png", proc{})
$button_panel.add_seperator()
$undo_icon = $button_panel.add_icon("../data/images/icons24/stock_undo.png", proc{$workspace.get_map().undo()})
$redo_icon = $button_panel.add_icon("../data/images/icons24/stock_redo.png", proc{$workspace.get_map().redo()})
$button_panel.add_seperator()


$tool_button_panel = ButtonPanel.new(320, 23, $screen.width, 33, true, $gui.get_component)
$tool_button_panel.add_seperator()
$tool_button_panel.add_icon("../data/images/icons24/object_raise.png", proc{
                         $objmap_select_tool.get_selection().each {|obj|
                           $workspace.get_map().get_data().objects.raise(obj)
                         }
                       })
$tool_button_panel.add_icon("../data/images/icons24/object_lower.png", proc{
                         $objmap_select_tool.get_selection().each {|obj|
                           $workspace.get_map().get_data().objects.lower(obj)
                         }
                       })
$tool_button_panel.show(false)

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
  $paint.set_down()
  $select.set_up()
  $zoom.set_up()
  $object.set_up()

  $brushbox.show(true)
  $objectselector.show(false)

  $tool_button_panel.show(false)
end

def set_tilemap_select_tool()
  $workspace.set_tool($tilemap_select_tool.to_tool())
  $paint.set_up()
  $select.set_down()
  $zoom.set_up()
  $object.set_up()

  $brushbox.show(false)
  $objectselector.show(false)

  $tool_button_panel.show(false)
end

def set_zoom_tool()
  $workspace.set_tool($zoom_tool.to_tool())
  $paint.set_up()
  $select.set_up()
  $zoom.set_down()
  $object.set_up()

  $brushbox.show(false)
  $objectselector.show(false)

  $tool_button_panel.show(false)
end

def set_objmap_select_tool()
  $workspace.set_tool($objmap_select_tool.to_tool())
  $paint.set_up()
  $select.set_up()
  $zoom.set_up()
  $object.set_down()

  $brushbox.show(false)
  $objectselector.show(true)

  $tool_button_panel.show(true)
end

$toolbar = Panel.new(CL_Rect.new(CL_Point.new(0, 23+33), CL_Size.new(33, 32*4+2)), $gui.get_component())

$paint = Icon.new(CL_Rect.new(CL_Point.new(2, 32*0+2), CL_Size.new(32, 32)), make_sprite("../data/images/tools/stock-tool-pencil-22.png"), "Some tooltip", $toolbar);
$paint.set_callback(method(:set_tilemap_paint_tool))

$select = Icon.new(CL_Rect.new(CL_Point.new(2, 32*1+2), CL_Size.new(32,32)), make_sprite("../data/images/tools/stock-tool-rect-select-22.png"), "Some tooltip", $toolbar);
$select.set_callback(method(:set_tilemap_select_tool))

$zoom = Icon.new(CL_Rect.new(CL_Point.new(2, 32*2+2), CL_Size.new(32,32)), make_sprite("../data/images/tools/stock-tool-zoom-22.png"), "Some tooltip", $toolbar);
$zoom.set_callback(method(:set_zoom_tool))

$object = Icon.new(CL_Rect.new(CL_Point.new(2, 32*3+2), CL_Size.new(32,32)), make_sprite("../data/images/tools/stock-tool-clone-22.png"), "Some tooltip", $toolbar);
$object.set_callback(method(:set_objmap_select_tool))

mysprite = make_sprite("../data/images/icons16/stock_paste-16.png")

def netpanzer_load_level(filename)
  level = Level.new(filename)
  level.activate($workspace)
  connect(level.editormap.sig_change(), proc{on_map_change})
  
#  if not(has_element($config.recent_files, filename))
#    $config.recent_files.push(filename)
#    recent_files_menu.add_item(mysprite, filename, proc{ netpanzer_load_level(filename) })
#  end
  $minimap.update_minimap()
end

def netpanzer_save_level(filename)
  $workspace.get_map().get_data().save(filename)
end

recent_files_menu = Menu.new(CL_Point.new(32*2, 54), $gui.get_component())
for filename in $config.recent_files
    recent_files_menu.add_item(mysprite, filename, proc{ netpanzer_load_level(filename) })
end

menu = CL_Menu.new($gui.get_component())
menu.add_item("File/New...", proc{gui_level_new})
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

def generate_sprites()
  $brushes.each_with_index{|(start, width, height, name), index|
    puts "#{index}"
    buffer = make_pixelbuffer(width * 32, height * 32)

    (0..(height-1)).each {|y|
      (0..(width-1)).each {|x|
        tile = $tileset.create(start + width * y + x)
        blit(buffer, tile.get_pixelbuffer(), x * 32, y * 32)
      }
    }
    
    CL_ProviderFactory.save(buffer, "sprites/#{index}.png")
  }
end

menu.add_item("Zoom/1:4 (25%) ",  proc{ gui_set_zoom(0.25) })
menu.add_item("Zoom/1:2 (50%) ",  proc{ gui_set_zoom(0.5) })
menu.add_item("Zoom/1:1 (100%) ", proc{ gui_set_zoom(1.0) })
menu.add_item("Zoom/2:1 (200%) ", proc{ gui_set_zoom(2.0) })
menu.add_item("Zoom/4:1 (400%) ", proc{ gui_set_zoom(4.0) })

menu.add_item("Scripts/Flatten",  proc{ $workspace.get_map().get_data().flatten() })
menu.add_item("Scripts/Unflatten",  proc{ $workspace.get_map().get_data().unflatten() })

$minimap = Minimap.new($editor_map, CL_Rect.new(CL_Point.new(3, ($screen.height-112)+56 - 128-3), CL_Size.new(128, 128)), $option_panel)

$load_dialog = SimpleFileDialog.new("Load netPanzer Level", "Load", "Cancel", $gui.get_component())
$load_dialog.set_filename($config.datadir + "maps/")
$save_dialog = SimpleFileDialog.new("Save netPanzer Level as...", "Save", "Cancel", $gui.get_component())
$save_dialog.set_filename($config.datadir + "maps/")

connect_v2($editor_map.sig_on_key("l"), proc{ |x, y|
             $objmap_select_tool.get_selection().each {|obj|
               $workspace.get_map().get_data().objects.raise(obj)
             }
           })
connect_v2($editor_map.sig_on_key("s"), proc{ |x, y| 
             $objmap_select_tool.get_selection().each {|obj|
               $workspace.get_map().get_data().objects.lower(obj)
             }
           })

set_tilemap_paint_tool()

gui_level_new()

# generate_sprites()
$gui.run()

# flexlay.deinit()
# print "deinit done"

# EOF #
