class BadGuy
  def initialize(type)
    @type = type
  end
end

class SuperTuxGUI
  quit_button = nil
  menu        = nil
  
  selector_window = nil
  tileselector    = nil
  objectselector  = nil

  def initialize(tileset, gui)
    @selector_window = Panel.new(CL_Rect.new(CL_Point.new(800-134, 23+33), CL_Size.new(128 + 6, 558)),
                                 gui.get_component())
    @tileselector = TileSelector.new(CL_Rect.new(CL_Point.new(3, 3), CL_Size.new(128, 552)), @selector_window)
    @tileselector.set_tileset(tileset)
    @tileselector.set_tiles((1..100).to_a)
    @tileselector.show(false)
    
    @objectselector = ObjectSelector.new(CL_Rect.new(0, 0, 128, 256), 42, 42, @selector_window)
    @objectselector.show(true)

    for object in $game_objects
      @objectselector.add_brush(ObjectBrush.new(make_sprite($datadir + object[1]),
                                                make_metadata(BadGuy.new(object[0]))))
    end
  end

  def show_objects()
    @tileselector.show(false)        
    @objectselector.show(true)
  end

  def show_tiles()
    @tileselector.show(true)        
    @objectselector.show(false)
  end

  def show_none()
    @tileselector.show(false)        
    @objectselector.show(false)
  end
end

def gui_level_save_as()
  save_dialog.set_filename(os.path.dirname(save_dialog.get_filename()) + "/")
  save_dialog.run(supertux_save_level)
end

def gui_level_save()
  if $workspace.get_map().get_metadata().filename
    save_dialog.set_filename($workspace.get_map().get_metadata().filename)
  else
    save_dialog.set_filename(os.path.dirname(save_dialog.get_filename())  + "/")
  end
  
  save_dialog.run(supertux_save_level)
end   

def gui_level_load()
  load_dialog.run(supertux_load_level)
end

def gui_toggle_minimap()
  if $minimap.is_visible()
    $minimap.show(false)
    $minimap_icon.set_up()
  else
    $minimap.show(true)
    $minimap_icon.set_down()
  end
end

def gui_toggle_grid()
  tilemap = $workspace.get_map().get_metadata().foreground;
  tilemap.set_draw_grid(!tilemap.get_draw_grid())

  if tilemap.get_draw_grid()
    grid_icon.set_down()
  else
    grid_icon.set_up()
  end
end

def on_map_change()
  if ($workspace.get_map().undo_stack_size() > 0)
    undo_icon.enable()
  else
    undo_icon.disable()
  end

  if ($workspace.get_map().redo_stack_size() > 0)
    redo_icon.enable()
  else
    redo_icon.disable()        
  end
end

def set_tilemap_paint_tool()
  $workspace.set_tool($tilemap_paint_tool.to_tool())
  $paint.set_down()
  $select.set_up()
  $zoom.set_up()
  $object.set_up()
  $supertux.show_tiles()
end

def set_tilemap_select_tool()
  $workspace.set_tool($tilemap_select_tool.to_tool())
  $paint.set_up()
  $select.set_down()
  $zoom.set_up()
  $object.set_up()
  $supertux.show_none()
end

def set_zoom_tool()
  $workspace.set_tool($zoom_tool.to_tool())
  $paint.set_up()
  $select.set_up()
  $zoom.set_down()
  $object.set_up()
  $supertux.show_none()
end

def set_objmap_select_tool()
  $workspace.set_tool($objmap_select_tool.to_tool())
  $paint.set_up()
  $select.set_up()
  $zoom.set_up()
  $object.set_down()
  $supertux.show_objects()
end

def gui_show_foreground()
  $display_properties.layer = FOREGROUND_LAYER
  $display_properties.set($workspace.get_map().get_metadata())
  TilemapLayer.set_current($workspace.get_map().get_metadata().foreground)
  $foreground_icon.set_down()
  $interactive_icon.set_up()
  $background_icon.set_up()
  $minimap.update_minimap()
end

def gui_show_background()
  $display_properties.layer = BACKGROUND_LAYER
  $display_properties.set($workspace.get_map().get_metadata())
  TilemapLayer.set_current($workspace.get_map().get_metadata().background)
  $foreground_icon.set_up()
  $interactive_icon.set_up()
  $background_icon.set_down()
  $minimap.update_minimap()
end

def gui_show_interactive()
  $display_properties.layer = INTERACTIVE_LAYER
  $display_properties.set($workspace.get_map().get_metadata())
  TilemapLayer.set_current($workspace.get_map().get_metadata().interactive)
  $foreground_icon.set_up()
  $interactive_icon.set_down()
  $background_icon.set_up()
  $minimap.update_minimap()
end

def gui_show_all()
  $display_properties.show_all = true
  $display_properties.current_only = false
  $display_properties.set($workspace.get_map().get_metadata())
end

def gui_show_current()
  $display_properties.show_all = false
  $display_properties.current_only = false
  $display_properties.set($workspace.get_map().get_metadata())
end

def gui_show_only_current()
  $display_properties.show_all = false
  $display_properties.current_only = true
  $display_properties.set($workspace.get_map().get_metadata())
end

def gui_toggle_display_props()
  if $display_properties.show_all
    $display_properties.show_all = false
  elsif not($display_properties.current_only)
    $display_properties.current_only = true
  else
    $display_properties.show_all = true
    $display_properties.current_only = false
  end
  
  $display_properties.set($workspace.get_map().get_metadata())    
end

def gui_resize_level()
  level = $workspace.get_map().get_data()
  dialog = GenericDialog.new("Resize Level", gui.get_component())
  dialog.add_int("Width: ", level.width)
  dialog.add_int("Height: ", level.height)
  dialog.add_int("X: ", 0)
  dialog.add_int("Y: ", 0)
  dialog.set_callback(proc{|w, h, x, y| 
                        level.resize(CL_Size.new(w, h), CL_Point.new(x, y))})
end

def gui_resize_level_to_selection()
  level = $workspace.get_map().get_data()
  rect  = tilemap_select_tool.get_selection_rect()
  if (rect.get_width() > 2 and rect.get_height() > 2)
    level.resize(rect.get_size(), CL_Point.new(-rect.left, -rect.top))
  end
end

def supertux_load_level(filename)
  print "Loading: ", filename
  level = Level(filename)
  level.activate(workspace)
  
  if not(config.recent_files.find(filename)) then
    config.recent_files.append(filename)
    recent_files_menu.add_item($mysprite, filename, 
                               proc { supertux_load_level(filename) })
  end

  minimap.update_minimap()
end

def gui_set_zoom(zoom)
  gc = $editor_map.get_workspace().get_gc_state()
  pos = gc.get_pos()
  gc.set_zoom(zoom)
  gc.set_pos(pos)
end

def menu_file_open()
  print "File/Open"
  level = Level('/home/ingo/cvs/supertux/supertux/data/levels/world1/level2.stl')
  print "Loading done"
  level.activate(workspace)
  connect(level.editormap.sig_change(), on_map_change)
  print "Activation done"
end

def supertux_save_level(filename)
  $workspace.get_map().get_metadata().save(filename)
end

def gui_switch_sector_menu()
  mymenu = Menu.new(CL_Point.new(530, 54), gui.get_component())
  for i in $workspace.get_map().get_metadata().parent.get_sectors()
    mymenu.add_item($mysprite, "Sector (%s)" % i,
                    proc { $workspace.get_map().get_metadata().parent.activate_sector(i, workspace) })
  end
  mymenu.run()
end

def gui_add_sector()
  level = $workspace.get_map().get_data().get_level()
  dialog = GenericDialog.new("Add Sector", gui.get_component())

  name = "newsector"
  width  = 50
  height = 20
  
  dialog.add_string("Name: ", name)
  dialog.add_int("Width: ",   width)
  dialog.add_int("Height: ",  height)
  
  dialog.set_callback(proc { |name, w, h|
                        sector = Sector().new(w, h)
                        sector.name = name
                        level.add_sector(sector) })
end  

# EOF #
