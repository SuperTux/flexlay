args = []

if args == []
  startlevel = Level.new(100, 50)
  startlevel.activate($workspace)
else
  supertux_load_level(args[0])
end

button_panel = Panel.new(CL_Rect.new(CL_Point.new(0, 23), CL_Size.new(800, 33)), $gui.get_component())

class Counter
  counter = 0;
  
  def initialize(i)
    @counter = i
  end
  
  def inc(i)
    @counter += i
    return @counter
  end
end

p = Counter.new(2)

$new_icon = Icon.new(CL_Rect.new(CL_Point.new(p.inc(0),  2), CL_Size.new(32, 32)),
                    make_sprite("../data/images/icons24/stock_new.png"), "Some tooltip", button_panel);
$load_icon = Icon.new(CL_Rect.new(CL_Point.new(p.inc(32), 2), CL_Size.new(32, 32)),
                     make_sprite("../data/images/icons24/stock_open.png"), "Some tooltip", button_panel);
$load_recent_icon = Icon.new(CL_Rect.new(CL_Point.new(p.inc(32), 2), CL_Size.new(16, 32)),
                        make_sprite("../data/images/icons24/downarrow.png"), "Some tooltip", button_panel);
$save_icon        = Icon.new(CL_Rect.new(CL_Point.new(p.inc(16), 2), CL_Size.new(32, 32)),
                        make_sprite("../data/images/icons24/stock_save.png"), "Some tooltip", button_panel);
$save_as_icon     = Icon.new(CL_Rect.new(CL_Point.new(p.inc(32), 2), CL_Size.new(32, 32)),
                        make_sprite("../data/images/icons24/stock_save_as.png"), "Some tooltip", button_panel);

$load_icon.set_callback(proc{ gui_level_load() })
$load_recent_icon.set_callback(proc{ $recent_files_menu.run() })
$save_icon.set_callback(proc{ gui_level_save() })
$save_as_icon.set_callback(proc{ gui_level_save_as() })

$copy_icon    = Icon.new(CL_Rect.new(CL_Point.new(p.inc(48), 2), CL_Size.new(32, 32)),
                    make_sprite("../data/images/icons24/stock_copy.png"), "Some tooltip", button_panel);
$paste_icon   = Icon.new(CL_Rect.new(CL_Point.new(p.inc(32), 2), CL_Size.new(32, 32)),
                    make_sprite("../data/images/icons24/stock_paste.png"), "Some tooltip", button_panel);

$undo_icon = Icon.new(CL_Rect.new(CL_Point.new(p.inc(48), 2), CL_Size.new(32, 32)),
                 make_sprite("../data/images/icons24/stock_undo.png"), "Some tooltip", button_panel);
$redo_icon = Icon.new(CL_Rect.new(CL_Point.new(p.inc(32), 2), CL_Size.new(32, 32)),
                 make_sprite("../data/images/icons24/stock_redo.png"), "Some tooltip", button_panel);

$undo_icon.set_callback(proc{ $workspace.get_map().undo() })
$redo_icon.set_callback(proc{ $workspace.get_map().redo() })

$undo_icon.disable()
$redo_icon.disable()

$minimap_icon = Icon.new(CL_Rect.new(CL_Point.new(p.inc(48), 2), CL_Size.new(32, 32)),
                    make_sprite("../data/images/icons24/minimap.png"), "Some tooltip", button_panel);
$minimap_icon.set_callback(proc{ gui_toggle_minimap() })

$grid_icon = Icon.new(CL_Rect.new(CL_Point.new(p.inc(32), 2), CL_Size.new(32, 32)),
                 make_sprite("../data/images/icons24/grid.png"), "Some tooltip", button_panel);
$grid_icon.set_callback(proc{ gui_toggle_grid() })

$background_icon  = Icon.new(CL_Rect.new(CL_Point.new(p.inc(48), 2), CL_Size.new(32, 32)),
                        make_sprite("../data/images/icons24/background.png"), "Some tooltip", button_panel);
$interactive_icon = Icon.new(CL_Rect.new(CL_Point.new(p.inc(32), 2), CL_Size.new(32, 32)),
                        make_sprite("../data/images/icons24/interactive.png"), "Some tooltip", button_panel);
$foreground_icon  = Icon.new(CL_Rect.new(CL_Point.new(p.inc(32), 2), CL_Size.new(32, 32)),
                        make_sprite("../data/images/icons24/foreground.png"), "Some tooltip", button_panel);
$eye_icon         = Icon.new(CL_Rect.new(CL_Point.new(p.inc(32), 2), CL_Size.new(32, 32)),
                        make_sprite("../data/images/icons24/eye.png"), "Some tooltip", button_panel);

$sector_icon         = Icon.new(CL_Rect.new(CL_Point.new(p.inc(48), 2), CL_Size.new(32, 32)),
                               make_sprite("../data/images/icons24/sector.png"), "Some tooltip", button_panel);

$sector_icon.set_callback(proc{ gui_switch_sector_menu() })

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

$foreground_icon.set_callback(proc{ gui_show_foreground() })
$interactive_icon.set_callback(proc{ gui_show_interactive() })
$background_icon.set_callback(proc{ gui_show_background() })
$eye_icon.set_callback(proc{ $layer_menu.run() })

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

$menu.add_item("Edit/Resize", proc{ gui_resize_level() })
$menu.add_item("Edit/Resize to selection", proc{ gui_resize_level_to_selection()})
$menu.add_item("Edit/Debug Shell", proc{ run_python()})
$menu.add_item("Edit/Add Sector...", proc{ gui_add_sector()})

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

class PathNode
  node = nil
  
  def initialize(node)
    @node = node
  end
end

def insert_path_node(x,y)
  print "Insert path Node"
  m = $workspace.get_map().get_metadata()
  pathnode = ObjMapPathNode($editor_map.screen2world(CL_Point.new(x, y)),
                            make_metadata("PathNode"))
  pathnode.to_object().set_metadata(make_metadata(PathNode(pathnode)))
  m.objects.add_object(pathnode.to_object())
end

def connect_path_nodes()
  print "Connecting path nodes"
  pathnodes = []
  for i in objmap_select_tool.get_selection()
    obj = get_python_object(i.get_metadata())
    if obj.__class__ == PathNode
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

# EOF #
