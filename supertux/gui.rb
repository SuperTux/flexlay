class SuperTuxGUI
  quit_button = nil
  menu        = nil
  
  selector_window = nil
  tileselector    = nil
  objectselector  = nil

  attr_reader :tileselector, :editor_map, :workspace, :minimap, :recent_files_menu, :gui

  def get_component()
    return @gui.get_component()
  end

  def initialize()
    @editor = Editor.new()
    @gui    = @editor.get_gui_manager()

    @display_properties = DisplayProperties.new()

    myrect      = CL_Rect.new(CL_Point.new(0, 56), CL_Size.new(665, 488+56))
    @editor_map = EditorMapComponent.new(myrect, @gui.get_component())
    @workspace  = Workspace.new(myrect.get_width(), myrect.get_height())
    @editor_map.set_workspace(@workspace)
    @workspace.set_tool($tilemap_paint_tool.to_tool());
    @minimap = Minimap.new(@editor_map, CL_Rect.new(CL_Point.new(3 + myrect.left, 
                                                                 488+3-14  + myrect.top), 
                                                    CL_Size.new(794-134-16, 50)), 
                           @gui.get_component())

    @selector_window = Panel.new(CL_Rect.new(CL_Point.new(800-134, 23+33), CL_Size.new(128 + 6, 558)),
                                 @gui.get_component())
    @tileselector = TileSelector.new(CL_Rect.new(CL_Point.new(3, 3), CL_Size.new(128, 552)), @selector_window)
    @tileselector.set_tileset($tileset)
    @tileselector.set_tiles($tileset.get_tiles())
    @tileselector.show(false)
    
    @objectselector = ObjectSelector.new(CL_Rect.new(0, 0, 128, 552), 42, 42, @selector_window)
    @objectselector.show(true)

    # connect_v1_ObjMapObject
    connect_v2_ObjectBrush_Point(@objectselector.sig_drop(), method(:on_object_drop))

    $game_objects.each { |object|
      @objectselector.add_brush(ObjectBrush.new(make_sprite($datadir + object[1]),
                                                make_metadata(object)))
    }

    create_button_panel()

    # FIXME: Having position in the Menus here is EXTREMLY ugly
    @tilegroup_menu = Menu.new(CL_Point.new(35*15+2, 54), @gui.get_component())
    @tilegroup_menu.add_item($mysprite, "All Tiles", proc{@tileselector.set_tiles($tileset.get_tiles())})
    $tileset.tilegroups.each { |tilegroup|
      @tilegroup_menu.add_item($mysprite, tilegroup.name, proc{@tileselector.set_tiles(tilegroup.tiles)})
    }

    @recent_files_menu = Menu.new(CL_Point.new(32*2, 54), @gui.get_component())

    @layer_menu = Menu.new(CL_Point.new(32*15+2, 54), @gui.get_component())
    @layer_menu.add_item($mysprite, "Show all", proc{ gui_show_all() })
    @layer_menu.add_item($mysprite, "Show current", proc{ gui_show_current() })
    @layer_menu.add_item($mysprite, "Show only current", proc{ gui_show_only_current() })

    # FIXME: Use ButtonPanel here instead
    @toolbar = Panel.new(CL_Rect.new(CL_Point.new(0, 23+33), CL_Size.new(33, 32*5+2)), @gui.get_component())

    @paint = Icon.new(CL_Rect.new(CL_Point.new(2, 32*0+2), CL_Size.new(32, 32)), make_sprite("../data/images/tools/stock-tool-pencil-22.png"), "Some tooltip", @toolbar);
    @paint.set_callback(proc{ set_tilemap_paint_tool() })

    @select = Icon.new(CL_Rect.new(CL_Point.new(2, 32*1+2), CL_Size.new(32,32)), make_sprite("../data/images/tools/stock-tool-rect-select-22.png"), "Some tooltip", @toolbar);
    @select.set_callback(proc{ set_tilemap_select_tool() })

    @zoom = Icon.new(CL_Rect.new(CL_Point.new(2, 32*2+2), CL_Size.new(32,32)), make_sprite("../data/images/tools/stock-tool-zoom-22.png"), "Some tooltip", @toolbar);
    @zoom.set_callback(proc{ set_zoom_tool() })

    @object = Icon.new(CL_Rect.new(CL_Point.new(2, 32*3+2), CL_Size.new(32,32)), make_sprite("../data/images/tools/stock-tool-clone-22.png"), "Some tooltip", @toolbar);
    @object.set_callback(proc{ set_objmap_select_tool() })

#     @stroke = Icon.new(CL_Rect.new(CL_Point.new(2, 32*4+2), CL_Size.new(32,32)), make_sprite("../data/images/tools/stock-tool-pencil-22.png"), "Some tooltip", @toolbar);
#     @stroke.set_callback(proc{ set_sketch_stroke_tool() })

    create_menu()

    @load_dialog = SimpleFileDialog.new("Load SuperTux Level", "Load", "Cancel", @gui.get_component())
    @load_dialog.set_filename($datadir + "levels/")
    @save_dialog = SimpleFileDialog.new("Save SuperTux Level as...", "Save", "Cancel", @gui.get_component())
    @save_dialog.set_filename($datadir + "levels/")

    register_keyboard_shortcuts()

    # Popup menu
    connect_v2($objmap_select_tool.sig_on_right_click(), proc{|x,y|
                 puts "Launching Menu at #{x}, #{y}"
                 menu = Menu.new(CL_Point.new(x, y), @gui.get_component())
                 menu.add_item($mysprite, "Delete Object(s)", proc{ 
                                 puts "Trying to delete #{@workspace.get_map().get_metadata()} #{@workspace.get_map().get_metadata().objects}"
                                 cmd = ObjectDeleteCommand.new(@workspace.get_map().get_metadata().objects)
                                 $objmap_select_tool.get_selection().each { |i| cmd.add_object(i) }
                                 @workspace.get_map().execute(cmd.to_command())
                                 $objmap_select_tool.clear_selection()
                               })
                 menu.add_item($mysprite, "Edit Properties", proc{
                                 $objmap_select_tool.get_selection().each {|i|
                                   puts i
                                   puts i.get_data()
                                   # FIXME: Not sure why we need get_ruby_object() here
                                   get_ruby_object(i.get_data()).property_dialog()
                                 }
                               })
                 menu.run()
               })
  end

  def register_keyboard_shortcuts()
    connect_v2(@editor_map.sig_on_key("f1"), proc{ |x, y| gui_toggle_minimap()})
    connect_v2(@editor_map.sig_on_key("m"),  proc{ |x, y| gui_toggle_minimap()})
    connect_v2(@editor_map.sig_on_key("g"),  proc{ |x, y| gui_toggle_grid()})
    connect_v2(@editor_map.sig_on_key("4"),  proc{ |x, y| gui_toggle_display_props()})
    connect_v2(@editor_map.sig_on_key("3"),  proc{ |x, y| gui_show_foreground()})
    connect_v2(@editor_map.sig_on_key("2"),  proc{ |x, y| gui_show_interactive()})
    connect_v2(@editor_map.sig_on_key("1"),  proc{ |x, y| gui_show_background()})
    
    connect_v2(@editor_map.sig_on_key("5"),  proc{ |x, y| @editor_map.zoom_in(CL_Point.new(x, y))})
    connect_v2(@editor_map.sig_on_key("6"),  proc{ |x, y| @editor_map.zoom_out(CL_Point.new(x, y))})
    
    connect_v2(@editor_map.sig_on_key("i"),  proc{ |x, y| insert_path_node(x,y)})
    connect_v2(@editor_map.sig_on_key("c"),  proc{ |x, y| connect_path_nodes()})
    
    connect_v2(@editor_map.sig_on_key("7"),  proc{ |x, y| @workspace.get_map().get_metadata().parent.activate_sector("main", @workspace)})
    connect_v2(@editor_map.sig_on_key("8"),  proc{ |x, y| @workspace.get_map().get_metadata().parent.activate_sector("another_world", @workspace)})
    
    connect_v2(@editor_map.sig_on_key("e"),  proc{ |x, y| gui_show_object_properties()})


    connect_v2(@editor_map.sig_on_key("a"),  proc { |x, y|
                 pos = @editor_map.screen2world(CL_Point.new(x, y))
                 rectobj = ObjMapRectObject.new(CL_Rect.new(pos,
                                                            CL_Size.new(128, 64)),
                                                CL_Color.new(0, 255, 255, 155),
                                                make_metadata(nil))
                 # rectobj.set_metadata(metadata)
                 @workspace.get_map().get_metadata().objects.add_object(rectobj.to_object())
               })
  end

  def create_menu()
    @menu = CL_Menu.new(@gui.get_component())
    @menu.add_item("File/Open...", method(:gui_level_load))
    @menu.add_item("File/Save...", method(:gui_level_save))
    # @menu.add_item("File/Save Commands...", menu_file_save_commands)
    @menu.add_item("File/Save As...", method(:gui_level_save_as))
    @menu.add_item("File/Quit",  proc{ @gui.quit })
    
    @menu.add_item("Edit/Smooth Selection", method(:gui_smooth_level_struct))
    @menu.add_item("Edit/Resize", method(:gui_resize_level))
    @menu.add_item("Edit/Resize to selection", method(:gui_resize_level_to_selection))
    @menu.add_item("Edit/Debug Shell", proc{ run_python()})
    @menu.add_item("Edit/Add Sector...", method(:gui_add_sector))
    @menu.add_item("Edit/Remove Current Sector", method(:gui_remove_sector))
    @menu.add_item("Edit/Sector Properties", method(:gui_edit_sector))
    @menu.add_item("Edit/Level Properties", method(:gui_edit_level))
    
    @menu.add_item("Zoom/1:4 (25%) ",  proc{ self.gui_set_zoom(0.25) })
    @menu.add_item("Zoom/1:2 (50%) ",  proc{ self.gui_set_zoom(0.5) })
    @menu.add_item("Zoom/1:1 (100%) ", proc{ self.gui_set_zoom(1.0) }) 
    @menu.add_item("Zoom/2:1 (200%) ", proc{ self.gui_set_zoom(2.0) })
    @menu.add_item("Zoom/4:1 (400%) ", proc{ self.gui_set_zoom(4.0) })
  end

  def create_button_panel()
    # button_panel = Panel.new(CL_Rect.new(CL_Point.new(0, 23), CL_Size.new(800, 33)), @gui.get_component())
    button_panel = ButtonPanel.new(0, 23, 800, 33, true, @gui.get_component)
    
    # File Handling
    button_panel.add_icon("../data/images/icons24/stock_new.png")
    button_panel.add_icon("../data/images/icons24/stock_open.png", proc{ self.gui_level_load() })
    button_panel.add_small_icon("../data/images/icons24/downarrow.png", proc{ @recent_files_menu.run() })
    button_panel.add_icon("../data/images/icons24/stock_save.png", proc{ self.gui_level_save() })
    button_panel.add_icon("../data/images/icons24/stock_save_as.png", proc{ self.gui_level_save_as() })

    # Copy&Paste
    button_panel.add_seperator()
    button_panel.add_icon("../data/images/icons24/stock_copy.png")
    button_panel.add_icon("../data/images/icons24/stock_paste.png")

    # Undo Redo
    button_panel.add_seperator()
    @undo_icon = button_panel.add_icon("../data/images/icons24/stock_undo.png", proc{ @workspace.get_map().undo() })
    @redo_icon = button_panel.add_icon("../data/images/icons24/stock_redo.png", proc{ @workspace.get_map().redo() })

    @undo_icon.disable()
    @redo_icon.disable()

    # Visibility Toggles
    button_panel.add_seperator()
    @minimap_icon = button_panel.add_icon("../data/images/icons24/minimap.png", proc{ gui_toggle_minimap() })
    @grid_icon    = button_panel.add_icon("../data/images/icons24/grid.png", proc{ gui_toggle_grid() })

    # Layers
    button_panel.add_seperator()
    @background_icon = button_panel.add_icon("../data/images/icons24/background.png", proc{ gui_show_background() })
    @interactive_icon = button_panel.add_icon("../data/images/icons24/interactive.png", proc{ gui_show_interactive() })
    @foreground_icon = button_panel.add_icon("../data/images/icons24/foreground.png", proc{ gui_show_foreground() })
    @eye_icon = button_panel.add_icon("../data/images/icons24/eye.png", proc{ @layer_menu.run() })

    button_panel.add_seperator()
    @sector_icon = button_panel.add_icon("../data/images/icons24/sector.png", proc{ gui_switch_sector_menu() })

    button_panel.add_seperator()
    @run_icon = button_panel.add_icon("../data/images/icons24/run.png", proc{ gui_run_level() })

    button_panel.add_icon("../data/images/icons24/eye.png", proc{ @tilegroup_menu.run() })
  end

  def on_object_drop(brush, pos)
    pos = @editor_map.screen2world(pos)
    data = get_ruby_object(brush.get_data())
    create_gameobject($gui.workspace.get_map().get_metadata().objects, data, pos)
  end

  def run()
    @gui.run()
  end

#   def show_colorpicker()
#     @tileselector.show(false)        
#     @objectselector.show(false)
# #    @colorpicker.show(true)
#   end

  def show_objects()
    @tileselector.show(false)        
    @objectselector.show(true)
#    @colorpicker.show(false)
  end

  def show_tiles()
    @tileselector.show(true)        
    @objectselector.show(false)
#    @colorpicker.show(false)
  end

  def show_none()
    @tileselector.show(false)        
    @objectselector.show(false)
#    @colorpicker.show(false)
  end

  def set_tilemap_paint_tool()
    @workspace.set_tool($tilemap_paint_tool.to_tool())
    @paint.set_down()
    @select.set_up()
    @zoom.set_up()
    @object.set_up()
    show_tiles()
  end

  def set_tilemap_select_tool()
    @workspace.set_tool($tilemap_select_tool.to_tool())
    @paint.set_up()
    @select.set_down()
    @zoom.set_up()
    @object.set_up()
    show_none()
  end

  def set_zoom_tool()
    @workspace.set_tool($zoom_tool.to_tool())
    @paint.set_up()
    @select.set_up()
    @zoom.set_down()
    @object.set_up()
    show_none()
  end

#   def set_sketch_stroke_tool()
#     @workspace.set_tool($sketch_stroke_tool.to_tool())
#     @paint.set_up()
#     @select.set_up()
#     @zoom.set_up()
#     @object.set_down()
#     show_colorpicker()
#   end

  def set_objmap_select_tool()
    @workspace.set_tool($objmap_select_tool.to_tool())
    @paint.set_up()
    @select.set_up()
    @zoom.set_up()
    @object.set_down()
    show_objects()
  end

  def gui_show_foreground()
    @display_properties.layer = FOREGROUND_LAYER
    @display_properties.set(@workspace.get_map().get_metadata())
    TilemapLayer.set_current(@workspace.get_map().get_metadata().foreground)
    @foreground_icon.set_down()
    @interactive_icon.set_up()
    @background_icon.set_up()
    @minimap.update_minimap()
  end

  def gui_show_background()
    @display_properties.layer = BACKGROUND_LAYER
    @display_properties.set(@workspace.get_map().get_metadata())
    TilemapLayer.set_current(@workspace.get_map().get_metadata().background)
    @foreground_icon.set_up()
    @interactive_icon.set_up()
    @background_icon.set_down()
    @minimap.update_minimap()
  end

  def gui_show_interactive()
    @display_properties.layer = INTERACTIVE_LAYER
    @display_properties.set(@workspace.get_map().get_metadata())
    TilemapLayer.set_current(@workspace.get_map().get_metadata().interactive)
    @foreground_icon.set_up()
    @interactive_icon.set_down()
    @background_icon.set_up()
    @minimap.update_minimap()
  end

  def gui_show_all()
    @display_properties.show_all = true
    @display_properties.current_only = false
    @display_properties.set(@workspace.get_map().get_metadata())
  end

  def gui_show_current()
    @display_properties.show_all = false
    @display_properties.current_only = false
    @display_properties.set(@workspace.get_map().get_metadata())
  end

  def gui_show_only_current()
    @display_properties.show_all = false
    @display_properties.current_only = true
    @display_properties.set(@workspace.get_map().get_metadata())
  end

  def gui_toggle_minimap()
    if @minimap.is_visible() then
      @minimap.show(false)
      @minimap_icon.set_up()
    else
      @minimap.show(true)
      @minimap_icon.set_down()
    end
  end

  def gui_toggle_grid()
    tilemap = @workspace.get_map().get_metadata().foreground;
    tilemap.set_draw_grid(!tilemap.get_draw_grid())
    
    if tilemap.get_draw_grid()
      @grid_icon.set_down()
    else
      @grid_icon.set_up()
    end
  end
  
  def gui_toggle_display_props()
    if @display_properties.show_all
      @display_properties.show_all = false
    elsif not(@display_properties.current_only)
      @display_properties.current_only = true
    else
      @display_properties.show_all = true
      @display_properties.current_only = false
    end
    
    @display_properties.set(@workspace.get_map().get_metadata())    
  end

  def gui_run_level()
    puts "Run this level..."
    # FIXME: use real tmpfile
    tmpfile = "/tmp/tmpflexlay-supertux.stl"
    supertux_save_level(tmpfile)
    # FIXME: doesn't work with latest supertux...
    fork { exec("#{$datadir}/../supertux", tmpfile) }
  end

  def gui_resize_level()
    level = @workspace.get_map().get_metadata()
    dialog = GenericDialog.new("Resize Level", @gui.get_component())
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
    level = @workspace.get_map().get_metadata()
    rect  = $tilemap_select_tool.get_selection_rect()
    if (rect.get_width() > 2 and rect.get_height() > 2)
      level.resize(rect.get_size(), CL_Point.new(-rect.left, -rect.top))
    end
  end

  def gui_edit_level()
    level = @workspace.get_map().get_metadata().get_level()
    dialog = GenericDialog.new("Edit Sector", @gui.get_component())

    dialog.add_string("Name:", level.name)
    dialog.add_string("Author:", level.author)
    dialog.add_int("Time:", level.time)

    dialog.set_block() { |name, author, time|
      level.name   = name
      level.author = author
      level.time   = time
    }
  end

  def gui_edit_sector()
    level = @workspace.get_map().get_metadata().get_level()
    dialog = GenericDialog.new("Edit Sector", @gui.get_component())
    
    dialog.add_string("Name: ",   level.current_sector.name)
    dialog.add_string("Music: ",   level.current_sector.music)
    dialog.add_float("Gravity: ", level.current_sector.gravity)
    
    dialog.set_block() { |name, music, gravity|
      level.current_sector.name = name
      level.current_sector.music = music
      level.current_sector.gravity = gravity
    }
  end

  def gui_set_zoom(zoom)
    gc = @editor_map.get_workspace().get_gc_state()
    pos = gc.get_pos()
    gc.set_zoom(zoom)
    gc.set_pos(pos)
  end

  def gui_remove_sector()
    sector = @workspace.get_map().get_metadata()
    sector.get_level().remove_sector(sector.name)
  end

  def gui_add_sector()
    level = @workspace.get_map().get_metadata().get_level()
    dialog = GenericDialog.new("Add Sector", @gui.get_component())
    
    dialog.add_string("Name: ", "newsector")
    dialog.add_string("Music: ",   "")
    dialog.add_float("Gravity: ", "10.0")
    dialog.add_int("Width: ",   30)
    dialog.add_int("Height: ",  20)
    
    dialog.set_callback(proc { |name, music, gravity, width, height|
                          uniq_name = name
                          i = 1
                          while level.get_sectors().index(uniq_name)
                            uniq_name = name + "<%d>" % i
                            i += 1
                          end

                          sector = Sector.new(level)
                          sector.new_from_size(uniq_name, width, height)

                          sector.song    = ""
                          sector.gravity = "10.0"

                          level.add_sector(sector) 
                        })
  end  

  def gui_switch_sector_menu()
    mymenu = Menu.new(CL_Point.new(530, 54), @gui.get_component())
    sector = @workspace.get_map().get_metadata()
    sector.parent.get_sectors().each do |i|
      if sector.name == i then
        current = " [current]"
      else
        current = ""
      end
      mymenu.add_item($mysprite, ("Sector (%s)%s" % [i, current]), proc { 
                        print "Switching to %s\n" % i
                        @workspace.get_map().get_metadata().parent.activate_sector(i, @workspace) 
                      })
    end
    mymenu.run()
  end

  def gui_show_object_properties()
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

  def on_map_change()
    if (@workspace.get_map().undo_stack_size() > 0)
      @undo_icon.enable()
    else
      @undo_icon.disable()
    end

    if (@workspace.get_map().redo_stack_size() > 0)
      @redo_icon.enable()
    else
      @redo_icon.disable()        
    end
  end

  def gui_level_save_as()
    filename = @save_dialog.get_filename()
    if filename[-1] == "/"[0]
      @save_dialog.set_filename(filename)
    else
      @save_dialog.set_filename(File.dirname(filename) + "/")
    end
    @save_dialog.run(proc{|filename| supertux_save_level(filename) })
  end

  def gui_level_save()
    filename = @workspace.get_map().get_metadata().parent.filename
    print "Filename: ", filename, "\n"
    if filename
      @save_dialog.set_filename(filename)
    else
      filename = @save_dialog.get_filename()
      if filename[-1] == "/"[0]
        @save_dialog.set_filename(filename)
      else
        @save_dialog.set_filename(File.dirname(filename) + "/")
      end
    end
    
    @save_dialog.run(proc{|filename| supertux_save_level(filename) })
  end   

  def gui_level_load()
    @load_dialog.run(proc{|filename| supertux_load_level(filename) })
  end

  def insert_path_node(x,y)
    print "Insert path Node"
    m = @workspace.get_map().get_metadata()
    pathnode = ObjMapPathNode.new(@editor_map.screen2world(CL_Point.new(x, y)),
                                  make_metadata("PathNode"))
    pathnode.to_object().set_metadata(make_metadata(PathNode.new(pathnode)))
    m.objects.add_object(pathnode.to_object())
  end

  def connect_path_nodes()
    print "Connecting path nodes"
    pathnodes = []
    for i in $objmap_select_tool.get_selection()
      obj = i.get_metadata()
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
end

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


def supertux_load_level(filename)
  print "Loading: ", filename, "\n"
  level = Level.new(filename)
  level.activate($gui.workspace)
  
  if not($recent_files.find{|el| el == filename}) then
    $recent_files.push(filename)
    $gui.recent_files_menu.add_item($mysprite, filename, 
                                proc { supertux_load_level(filename) })
  end
  
  $gui.minimap.update_minimap()
end

def supertux_save_level(filename)
  level = $gui.workspace.get_map().get_metadata().parent
  # Do backup save
  if File.exists?(filename) then
    File.rename(filename, filename + "~")
  end
  level.save(filename)
  level.filename = filename
end

# EOF #
