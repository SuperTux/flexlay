class SuperTuxGUI
  quit_button = nil
  menu        = nil
  
  selector_window = nil
  tileselector    = nil
  objectselector  = nil

  def initialize(tileset, gui)
    @selector_window = Panel.new(CL_Rect.new(CL_Point.new(800-134, 23+33), CL_Size.new(128 + 6, 558)),
                                 $gui.get_component())
    @tileselector = TileSelector.new(CL_Rect.new(CL_Point.new(3, 3), CL_Size.new(128, 552)), @selector_window)
    @tileselector.set_tileset(tileset)
    @tileselector.set_tiles(tileset.get_tiles())
    @tileselector.show(false)
    
    @objectselector = ObjectSelector.new(CL_Rect.new(0, 0, 128, 256), 42, 42, @selector_window)
    @objectselector.show(true)

    connect_v1_ObjMapObject(@objectselector.sig_drop(), method(:on_object_drop))

    $game_objects.each do |object|
      @objectselector.add_brush(ObjectBrush.new(make_sprite($datadir + object[1]),
                                                make_metadata(object)))
    end
  end

  def on_object_drop(cppobj)
    # Get the metadata info and extract the generator call from it, see $game_objects
    print "on_object_drop:\n"
    metadata = get_ruby_object(cppobj.get_metadata())
    cppobj.set_metadata(make_metadata(metadata[2].call(cppobj)))
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
  filename = $save_dialog.get_filename()
  if filename[-1] == "/"[0]
    $save_dialog.set_filename(filename)
  else
    $save_dialog.set_filename(File.dirname(filename) + "/")
  end
  $save_dialog.run(proc{|filename| supertux_save_level(filename) })
end

def gui_level_save()
  filename = $workspace.get_map().get_metadata().parent.filename
  print "Filename: ", filename, "\n"
  if filename
    $save_dialog.set_filename(filename)
  else
    filename = $save_dialog.get_filename()
    if filename[-1] == "/"[0]
      $save_dialog.set_filename(filename)
    else
      $save_dialog.set_filename(File.dirname(filename) + "/")
    end
  end
  
  $save_dialog.run(proc{|filename| supertux_save_level(filename) })
end   

def gui_level_load()
  $load_dialog.run(proc{|filename| supertux_load_level(filename) })
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
    $grid_icon.set_down()
  else
    $grid_icon.set_up()
  end
end

def on_map_change()
  if ($workspace.get_map().undo_stack_size() > 0)
    $undo_icon.enable()
  else
    $undo_icon.disable()
  end

  if ($workspace.get_map().redo_stack_size() > 0)
    $redo_icon.enable()
  else
    $redo_icon.disable()        
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
  level = $workspace.get_map().get_metadata()
  dialog = GenericDialog.new("Resize Level", $gui.get_component())
  dialog.add_int("Width: ", level.width)
  dialog.add_int("Height: ", level.height)
  dialog.add_int("X: ", 0)
  dialog.add_int("Y: ", 0)
  dialog.set_callback(proc{|w, h, x, y| 
                        level.resize(CL_Size.new(w, h), CL_Point.new(x, y))})
end

def gui_smooth_level_struct()
  puts "Smoothing level structure"
  tilemap = TilemapLayer.current()
  data    = tilemap.get_data()
  width   = tilemap.get_width()
  height  = tilemap.get_height()

  get = proc { |x, y| return data[y*width + x] }
  set = proc { |x, y, val| data[y*width + x] = val }

  smooth = proc do |x, y|
    $itile_conditions.each do |ary|
      if (($solid_itiles.index(get[x-1,y-1]) ? 1 : 0) == ary[0] \
          and ($solid_itiles.index(get[x,  y-1]) ? 1 : 0) == ary[1] \
          and ($solid_itiles.index(get[x+1,y-1]) ? 1 : 0) == ary[2] \
          and ($solid_itiles.index(get[x-1,y  ]) ? 1 : 0) == ary[3] \
          and ($solid_itiles.index(get[x,  y  ]) ? 1 : 0) == ary[4] \
          and ($solid_itiles.index(get[x+1,y  ]) ? 1 : 0) == ary[5] \
          and ($solid_itiles.index(get[x-1,y+1]) ? 1 : 0) == ary[6] \
          and ($solid_itiles.index(get[x,  y+1]) ? 1 : 0) == ary[7] \
          and ($solid_itiles.index(get[x+1,y+1]) ? 1 : 0) == ary[8])
      then
        set[x,y, ary[9]]
      end
    end
  end

  rect  = $tilemap_select_tool.get_selection_rect()

  start_x = rect.left
  end_x   = rect.right
  start_y = rect.top
  end_y   = rect.bottom

  for y in (start_y..end_y) do
    for x in (start_x..end_x) do
      smooth[x,y]
    end
  end
  tilemap.set_data(data)
end

def gui_resize_level_to_selection()
  level = $workspace.get_map().get_metadata()
  rect  = $tilemap_select_tool.get_selection_rect()
  if (rect.get_width() > 2 and rect.get_height() > 2)
    level.resize(rect.get_size(), CL_Point.new(-rect.left, -rect.top))
  end
end

def supertux_load_level(filename)
  print "Loading: ", filename, "\n"
  level = Level.new(filename)
  level.activate($workspace)
  
  if not($recent_files.find{|el| el == filename}) then
    $recent_files.push(filename)
    $recent_files_menu.add_item($mysprite, filename, 
                                proc { supertux_load_level(filename) })
  end

  $minimap.update_minimap()
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
  print "Loading done\n"
  level.activate($workspace)
  connect(level.editormap.sig_change(), proc{on_map_change()})
  print "Activation done"
end

def supertux_save_level(filename)
  level = $workspace.get_map().get_metadata().parent
  # Do backup save
  if File.exists?(filename) then
    File.rename(filename, filename + "~")
  end
  level.save(filename)
  level.filename = filename
end

def gui_switch_sector_menu()
  mymenu = Menu.new(CL_Point.new(530, 54), $gui.get_component())
  sector = $workspace.get_map().get_metadata()
  sector.parent.get_sectors().each do |i|
    if sector.name == i then
      current = " [current]"
    else
      current = ""
    end
    mymenu.add_item($mysprite, ("Sector (%s)%s" % [i, current]), proc { 
                      print "Switching to %s\n" % i
                      $workspace.get_map().get_metadata().parent.activate_sector(i, $workspace) 
                    })
  end
  mymenu.run()
end

def gui_remove_sector()
  sector = $workspace.get_map().get_metadata()
  sector.get_level().remove_sector(sector.name)
end

def gui_add_sector()
  level = $workspace.get_map().get_metadata().get_level()
  dialog = GenericDialog.new("Add Sector", $gui.get_component())
 
  dialog.add_string("Name: ", "newsector")
  dialog.add_int("Width: ",   30)
  dialog.add_int("Height: ",  20)
  
  dialog.set_callback(proc { |name, width, height|
                        uniq_name = name
                        i = 1
                        while level.get_sectors().index(uniq_name)
                          uniq_name = name + "<%d>" % i
                          i += 1
                        end

                        sector = Sector.new(level)
                        sector.new_from_size(uniq_name, width, height)
                        level.add_sector(sector) 
                      })
end  

# EOF #
