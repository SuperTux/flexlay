args = []

if args == []
  startlevel = Level.new(100, 50)
  startlevel.activate($workspace)
else
  supertux_load_level(args[0])
end

# button_panel = Panel.new(CL_Rect.new(CL_Point.new(0, 23), CL_Size.new(800, 33)), $gui.get_component())
button_panel = ButtonPanel.new(0, 23, 800, 33, true, $gui.get_component)

# File Handling
button_panel.add_icon("../data/images/icons24/stock_new.png")
button_panel.add_icon("../data/images/icons24/stock_open.png", proc{ gui_level_load() })
button_panel.add_small_icon("../data/images/icons24/downarrow.png", proc{ $recent_files_menu.run() })
button_panel.add_icon("../data/images/icons24/stock_save.png", proc{ gui_level_save() })
button_panel.add_icon("../data/images/icons24/stock_save_as.png", proc{ gui_level_save_as() })

# Copy&Paste
button_panel.add_seperator()
button_panel.add_icon("../data/images/icons24/stock_copy.png")
button_panel.add_icon("../data/images/icons24/stock_paste.png")

# Undo Redo
button_panel.add_seperator()
$undo_icon = button_panel.add_icon("../data/images/icons24/stock_undo.png", proc{ $workspace.get_map().undo() })
$redo_icon = button_panel.add_icon("../data/images/icons24/stock_redo.png", proc{ $workspace.get_map().redo() })

$undo_icon.disable()
$redo_icon.disable()

# Visibility Toggles
button_panel.add_seperator()
$minimap_icon = button_panel.add_icon("../data/images/icons24/minimap.png", proc{ gui_toggle_minimap() })
$grid_icon    = button_panel.add_icon("../data/images/icons24/grid.png", proc{ gui_toggle_grid() })

# Layers
button_panel.add_seperator()
$background_icon = button_panel.add_icon("../data/images/icons24/background.png", proc{ gui_show_background() })
$interactive_icon = button_panel.add_icon("../data/images/icons24/interactive.png", proc{ gui_show_interactive() })
$foreground_icon = button_panel.add_icon("../data/images/icons24/foreground.png", proc{ gui_show_foreground() })
$eye_icon = button_panel.add_icon("../data/images/icons24/eye.png", proc{ $layer_menu.run() })

button_panel.add_seperator()
$sector_icon = button_panel.add_icon("../data/images/icons24/sector.png", proc{ gui_switch_sector_menu() })

button_panel.add_seperator()
$run_icon = button_panel.add_icon("../data/images/icons24/run.png", proc{ gui_run_level() })

button_panel.add_icon("../data/images/icons24/eye.png", proc{ $tilegroup_menu.run() })

# FIXME: Having position in the Menus here is EXTREMLY ugly
$tilegroup_menu = Menu.new(CL_Point.new(35*15+2, 54), $gui.get_component())
$tilegroup_menu.add_item($mysprite, "All Tiles", proc{$supertux.tileselector.set_tiles($tileset.get_tiles())})
$tileset.tilegroups.each { |tilegroup|
  $tilegroup_menu.add_item($mysprite, tilegroup.name, proc{$supertux.tileselector.set_tiles(tilegroup.tiles)})
}

$layer_menu = Menu.new(CL_Point.new(32*15+2, 54), $gui.get_component())

$toolbar = Panel.new(CL_Rect.new(CL_Point.new(0, 23+33), CL_Size.new(33, 32*4+2)), $gui.get_component())

$paint = Icon.new(CL_Rect.new(CL_Point.new(2, 32*0+2), CL_Size.new(32, 32)), make_sprite("../data/images/tools/stock-tool-pencil-22.png"), "Some tooltip", $toolbar);
$paint.set_callback(proc{ set_tilemap_paint_tool() })

$select = Icon.new(CL_Rect.new(CL_Point.new(2, 32*1+2), CL_Size.new(32,32)), make_sprite("../data/images/tools/stock-tool-rect-select-22.png"), "Some tooltip", $toolbar);
$select.set_callback(proc{ set_tilemap_select_tool() })

$zoom = Icon.new(CL_Rect.new(CL_Point.new(2, 32*2+2), CL_Size.new(32,32)), make_sprite("../data/images/tools/stock-tool-zoom-22.png"), "Some tooltip", $toolbar);
$zoom.set_callback(proc{ set_zoom_tool() })

$object = Icon.new(CL_Rect.new(CL_Point.new(2, 32*3+2), CL_Size.new(32,32)), make_sprite("../data/images/tools/stock-tool-clone-22.png"), "Some tooltip", $toolbar);
$object.set_callback(proc{ set_objmap_select_tool() })

# erase  = Icon.new(CL_Point.new(2, 32+1+2), make_sprite("../data/images/tools/stock-tool-eraser-22.png"), "Some tooltip", $toolbar);
# move   = Icon.new(CL_Point.new(2, 32*2+2), make_sprite("../data/images/tools/stock-tool-move-22.png"), "Some tooltip", $toolbar);

# SuperTux Specific stuff
$layer_menu.add_item($mysprite, "Show all", proc{ gui_show_all() })
$layer_menu.add_item($mysprite, "Show current", proc{ gui_show_current() })
$layer_menu.add_item($mysprite, "Show only current", proc{ gui_show_only_current() })

$supertux = SuperTuxGUI.new($tileset, $gui)

level = nil

$menu = CL_Menu.new($gui.get_component())
$menu.add_item("File/Open...", proc{ gui_level_load() })
$menu.add_item("File/Save...", proc{ gui_level_save() })
# $menu.add_item("File/Save Commands...", menu_file_save_commands)
$menu.add_item("File/Save As...", proc{ gui_level_save_as() })
$menu.add_item("File/Quit",  proc{ $gui.quit })

$menu.add_item("Edit/Smooth Selection", proc{ gui_smooth_level_struct() })
$menu.add_item("Edit/Resize", proc{ gui_resize_level() })
$menu.add_item("Edit/Resize to selection", proc{ gui_resize_level_to_selection()})
$menu.add_item("Edit/Debug Shell", proc{ run_python()})
$menu.add_item("Edit/Add Sector...", proc{ gui_add_sector()})
$menu.add_item("Edit/Remove Current Sector", proc{ gui_remove_sector()})
$menu.add_item("Edit/Sector Properties", proc{ gui_edit_sector()})
$menu.add_item("Edit/Level Properties", proc{ gui_edit_level()})

$menu.add_item("Zoom/1:4 (25%) ",  proc{ gui_set_zoom(0.25) })
$menu.add_item("Zoom/1:2 (50%) ",  proc{ gui_set_zoom(0.5) })
$menu.add_item("Zoom/1:1 (100%) ", proc{ gui_set_zoom(1.0) }) 
$menu.add_item("Zoom/2:1 (200%) ", proc{ gui_set_zoom(2.0) })
$menu.add_item("Zoom/4:1 (400%) ", proc{ gui_set_zoom(4.0) })

$display_properties = DisplayProperties.new()

$load_dialog = SimpleFileDialog.new("Load SuperTux Level", "Load", "Cancel", $gui.get_component())
$load_dialog.set_filename($datadir + "levels/")
$save_dialog = SimpleFileDialog.new("Save SuperTux Level as...", "Save", "Cancel", $gui.get_component())
$save_dialog.set_filename($datadir + "levels/")

# Init the GUI, so that button state is in sync with internal state
gui_toggle_minimap()
gui_toggle_minimap()
gui_show_interactive()
gui_show_current()
set_tilemap_paint_tool()

def insert_path_node(x,y)
  print "Insert path Node"
  m = $workspace.get_map().get_metadata()
  pathnode = ObjMapPathNode.new($editor_map.screen2world(CL_Point.new(x, y)),
                                make_metadata("PathNode"))
  pathnode.to_object().set_metadata(make_metadata(PathNode.new(pathnode)))
  m.objects.add_object(pathnode.to_object())
end

def connect_path_nodes()
  print "Connecting path nodes"
  pathnodes = []
  for i in $objmap_select_tool.get_selection()
    obj = get_ruby_object(i.get_metadata())
    if obj.is_a?(PathNode)
      pathnodes.push(obj.node)
    end
  end

  last = nil
  for i in pathnodes
    if last != nil:
            last.connect(i)
    end
    last = i
  end
end
            
def gui_show_object_properties()
  filename = $workspace.get_map().get_metadata().objects
  $objmap_select_tool.get_selection()
  selection = $objmap_select_tool.get_selection()
  if selection.length() > 1 then
    print "Warning: Selection to large"
  elsif selection.length() == 1 then
    obj = get_ruby_object(selection[0].get_metadata())
    obj.property_dialog()
  else
    print "Warning: Selection is empty\n"
  end
end

# Keyboard shortcuts
connect_v2($editor_map.sig_on_key("f1"), proc{ |x, y| gui_toggle_minimap()})
connect_v2($editor_map.sig_on_key("m"),  proc{ |x, y| gui_toggle_minimap()})
connect_v2($editor_map.sig_on_key("g"),  proc{ |x, y| gui_toggle_grid()})
connect_v2($editor_map.sig_on_key("4"),  proc{ |x, y| gui_toggle_display_props()})
connect_v2($editor_map.sig_on_key("3"),  proc{ |x, y| gui_show_foreground()})
connect_v2($editor_map.sig_on_key("2"),  proc{ |x, y| gui_show_interactive()})
connect_v2($editor_map.sig_on_key("1"),  proc{ |x, y| gui_show_background()})

connect_v2($editor_map.sig_on_key("5"),  proc{ |x, y| $editor_map.zoom_in(CL_Point.new(x, y))})
connect_v2($editor_map.sig_on_key("6"),  proc{ |x, y| $editor_map.zoom_out(CL_Point.new(x, y))})

connect_v2($editor_map.sig_on_key("i"),  proc{ |x, y| insert_path_node(x,y)})
connect_v2($editor_map.sig_on_key("c"),  proc{ |x, y| connect_path_nodes()})

connect_v2($editor_map.sig_on_key("7"),  proc{ |x, y| $workspace.get_map().get_metadata().parent.activate_sector("main", $workspace)})
connect_v2($editor_map.sig_on_key("8"),  proc{ |x, y| $workspace.get_map().get_metadata().parent.activate_sector("another_world", $workspace)})

connect_v2($editor_map.sig_on_key("e"),  proc{ |x, y| gui_show_object_properties()})

# EOF #
