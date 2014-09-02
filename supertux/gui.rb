require "flexlay.rb"

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

  def initialize(width, height)
    @gui = GUIManager.new()

    @display_properties = DisplayProperties.new()

    buttonpanel_rect = Rect.new(Point.new(0, 23), Size.new(width, 33))
    selector_rect = Rect.new(Point.new(width - 134, buttonpanel_rect.bottom),
                             Size.new(134, height-buttonpanel_rect.bottom))
    minimap_rect = Rect.new(Point.new(3, height-53),
                            Size.new(width - selector_rect.get_width() - 4, 50))
    map_rect = Rect.new(Point.new(0, buttonpanel_rect.bottom),
                        Size.new(width - selector_rect.get_width() - 1,
                                 height - buttonpanel_rect.bottom -
                                 minimap_rect.get_height() - 3))

    @editor_map = EditorMapComponent.new(map_rect.to_cl(), @gui.get_component())
    @workspace = @editor_map.get_workspace()

    @workspace.set_tool(0, $tilemap_paint_tool.to_tool())
    @workspace.set_tool(2, $workspace_move_tool.to_tool())
    @workspace.set_tool(1, $tilemap_paint_tool.to_tool())
    # 'x' key
    @workspace.set_tool(120, $zoom_tool.to_tool())
    # 'u' key
    @workspace.set_tool(117, $objmap_select_tool.to_tool())

    @workspace.set_tool(106, $workspace_move_tool.to_tool())
    @workspace.set_tool(107, $zoom2_tool.to_tool())
    @workspace.set_tool(65507, $zoom2_tool.to_tool())

    @minimap = Minimap.new(@editor_map, minimap_rect.to_cl(), @gui.get_component())

    @selector_window = Panel.new(selector_rect.to_cl(), @gui.get_component())
    @tileselector = TileSelector.new(CL_Rect.new(CL_Point.new(3, 3),
            CL_Size.new(selector_rect.get_width() -3 ,
                        selector_rect.get_height() - 3)), @selector_window)
    @tileselector.set_tileset($tileset)
    @tileselector.set_tiles($tileset.get_tiles())
    
    @objectselector = ObjectSelector.new(CL_Rect.new(CL_Point.new(3, 3),
                CL_Size.new(selector_rect.get_width()-3,
                            selector_rect.get_height() - 3)),
                42, 42, @selector_window)

    connect_v2_ObjectBrush_Point(@objectselector.sig_drop(), method(:on_object_drop))

    $game_objects.each { |objectdata|
      sprite = load_cl_sprite($datadir + objectdata[1])
      @objectselector.add_brush(ObjectBrush.new(sprite,
                                                make_metadata(objectdata)))
    }

    @worldmapobjectselector = ObjectSelector.new(CL_Rect.new(CL_Point.new(3, 3),
            CL_Size.new(selector_rect.get_width()-3,
                        selector_rect.get_height() - 3)),
            42, 42, @selector_window);
    connect_v2_ObjectBrush_Point(@worldmapobjectselector.sig_drop(),
            method(:on_worldmap_object_drop))
    $worldmap_objects.each { |object|
      @worldmapobjectselector.add_brush(ObjectBrush.new(
            make_sprite($datadir + object[1]),
            make_metadata(object[0])))
    }

    create_button_panel(buttonpanel_rect)
    @recent_files_menu = Menu.new(CL_Point.new(32*2, 54), @gui.get_component())

    @layer_menu = Menu.new(CL_Point.new(32*15+2, 54), @gui.get_component())
    @layer_menu.add_item($mysprite, "Show all", proc{ gui_show_all() })
    @layer_menu.add_item($mysprite, "Show current", proc{ gui_show_current() })
    @layer_menu.add_item($mysprite, "Show only current", proc{ gui_show_only_current() })

    # FIXME: Use ButtonPanel here instead
    @toolbar = Panel.new(CL_Rect.new(CL_Point.new(0, 23+33),
                                     CL_Size.new(33, 32*4+2)), @gui.get_component())

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

    @load_dialog = FileDialog.new("Load SuperTux Level", "Load", "Cancel", @gui.get_component())
    @load_dialog.set_filename($datadir + "levels/")
    @save_dialog = FileDialog.new("Save SuperTux Level as...", "Save", "Cancel", @gui.get_component())
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
                                   i.get_data().property_dialog()
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
                 pos = @editor_map.screen2world(Point.new(x, y))
                 rectobj = ObjMapRectObject.new(Rect.new(pos,
                                                         Size.new(128, 64)),
                                                CL_Color.new(0, 255, 255, 155),
                                                make_metadata(nil))
                 # rectobj.set_metadata(metadata)
                 @workspace.get_map().get_metadata().objects.add_object(rectobj.to_object())
               })
  end

  def create_menu()
    # @menu = @gui.create_menubar()
    @menu = CL_Menu.new(@gui.get_component())
    @menu.add_item("File/New...", method(:gui_level_new))
    @menu.add_item("File/Open...", method(:gui_level_load))
    @menu.add_item("File/Save...", method(:gui_level_save))
    # @menu.add_item("File/Save Commands...", menu_file_save_commands)
    # @menu.add_item("File/Save As...", method(:gui_level_save_as))
    @menu.add_item("File/Properties...", method(:gui_edit_level))
    @menu.add_item("File/Quit",  proc{ @gui.quit })
    
    @menu.add_item("Edit/Smooth Selection", method(:gui_smooth_level_struct))
    @menu.add_item("Edit/Resize", method(:gui_resize_level))
    @menu.add_item("Edit/Resize to selection", method(:gui_resize_level_to_selection))
    
    @menu.add_item("Zoom/1:4 (25%) ",  proc{ self.gui_set_zoom(0.25) })
    @menu.add_item("Zoom/1:2 (50%) ",  proc{ self.gui_set_zoom(0.5) })
    @menu.add_item("Zoom/1:1 (100%) ", proc{ self.gui_set_zoom(1.0) }) 
    @menu.add_item("Zoom/2:1 (200%) ", proc{ self.gui_set_zoom(2.0) })
    @menu.add_item("Zoom/4:1 (400%) ", proc{ self.gui_set_zoom(4.0) })
  end

  def create_button_panel(rect)
    button_panel = @gui.create_button_panel(rect, true)
    button_panel.init(rect.left, rect.top, rect.get_width(), rect.get_height(), true, @gui.get_component())
    
    # File Handling
    button_panel.add_icon("../data/images/icons24/stock_new.png",  proc{ self.gui_level_new() })
    button_panel.add_icon("../data/images/icons24/stock_open.png", proc{ self.gui_level_load() })
    button_panel.add_small_icon("../data/images/icons24/downarrow.png", proc{ @recent_files_menu.run() })
    button_panel.add_icon("../data/images/icons24/stock_save.png", proc{ self.gui_level_save() })
    button_panel.add_icon("../data/images/icons24/stock_save_as.png", proc{ self.gui_level_save_as() })

    # Copy&Paste
    button_panel.add_separator()
    button_panel.add_icon("../data/images/icons24/stock_copy.png")
    button_panel.add_icon("../data/images/icons24/stock_paste.png")

    # Undo Redo
    button_panel.add_separator()
    @undo_icon = button_panel.add_icon("../data/images/icons24/stock_undo.png", 
                                       proc{ @workspace.get_map().undo() })
    @redo_icon = button_panel.add_icon("../data/images/icons24/stock_redo.png", 
                                       proc{ @workspace.get_map().redo() })

    @undo_icon.disable()
    @redo_icon.disable()

    # Visibility Toggles
    button_panel.add_separator()
    @minimap_icon = button_panel.add_icon("../data/images/icons24/minimap.png", proc{ gui_toggle_minimap() })
    @grid_icon    = button_panel.add_icon("../data/images/icons24/grid.png", proc{ gui_toggle_grid() })

    # Layers
    button_panel.add_separator()
    @background_icon = button_panel.add_icon("../data/images/icons24/background.png", proc{ gui_show_background() })
    @interactive_icon = button_panel.add_icon("../data/images/icons24/interactive.png", proc{ gui_show_interactive() })
    @foreground_icon = button_panel.add_icon("../data/images/icons24/foreground.png", proc{ gui_show_foreground() })
    @eye_icon = button_panel.add_icon("../data/images/icons24/eye.png", proc{ @layer_menu.run() })

    button_panel.add_separator()
    @sector_icon = button_panel.add_icon("../data/images/icons24/sector.png", proc{ gui_switch_sector_menu() })

    button_panel.add_separator()
    @run_icon = button_panel.add_icon("../data/images/icons24/run.png", proc{ gui_run_level() })

    @tilegroup_icon = button_panel.add_icon("../data/images/icons24/eye.png", proc{ @tilegroup_menu.run() })

    @tilegroup_menu = Menu.new(CL_Point.new(@tilegroup_icon.get_screen_x(), 
                                            @tilegroup_icon.get_screen_y() + @tilegroup_icon.get_height() - 2),
                               @gui.get_component())
    @tilegroup_menu.add_item($mysprite, "All Tiles", proc{@tileselector.set_tiles($tileset.get_tiles())})
    $tileset.tilegroups.each { |tilegroup|
      @tilegroup_menu.add_item($mysprite, tilegroup.name, proc{@tileselector.set_tiles(tilegroup.tiles)})
    }
  end

  def on_worldmap_object_drop(brush, pos)
    pos = @editor_map.screen2world(pos)
    object_type = get_ruby_object(brush.get_data())
    create_worldmapobject_at_pos(
            $gui.workspace.get_map().get_metadata().objects, object_type, pos)
  end

  def on_object_drop(brush, pos)
    pos = @editor_map.screen2world(pos)
    data = get_ruby_object(brush.get_data())
    create_gameobject($gui.workspace.get_map(), $gui.workspace.get_map().get_metadata().objects, data, pos)
  end

  def run()
    @gui.run()
  end

#   def show_colorpicker()
#     @tileselector.show(false)        
#     @objectselector.show(false)
#     @worldmapobjectselector.show(false)
# #    @colorpicker.show(true)
#   end

  def show_objects()
    @tileselector.show(false)
    if $use_worldmap
      @worldmapobjectselector.show(true)
      @objectselector.show(false)
    else
      @worldmapobjectselector.show(false)
      @objectselector.show(true)
    end
#    @colorpicker.show(false)
  end

  def show_tiles()
    @tileselector.show(true)        
    @objectselector.show(false)
    @worldmapobjectselector.show(false)
#    @colorpicker.show(false)
  end

  def show_none()
    @tileselector.show(false)        
    @objectselector.show(false)
    @worldmapobjectselector.show(false)
#    @colorpicker.show(false)
  end

  def set_tilemap_paint_tool()
    @workspace.set_tool(0, $tilemap_paint_tool.to_tool())
    @workspace.set_tool(1, $tilemap_paint_tool.to_tool())
    @paint.set_down()
    @select.set_up()
    @zoom.set_up()
    @object.set_up()
    show_tiles()
  end

  def set_tilemap_select_tool()
    @workspace.set_tool(0, $tilemap_select_tool.to_tool())
    @paint.set_up()
    @select.set_down()
    @zoom.set_up()
    @object.set_up()
    show_none()
  end

  def set_zoom_tool()
    @workspace.set_tool(0, $zoom_tool.to_tool())
    @paint.set_up()
    @select.set_up()
    @zoom.set_down()
    @object.set_up()
    show_none()
  end

#   def set_sketch_stroke_tool()
#     @workspace.set_tool(0, $sketch_stroke_tool.to_tool())
#     @paint.set_up()
#     @select.set_up()
#     @zoom.set_up()
#     @object.set_down()
#     show_colorpicker()
#   end

  def set_objmap_select_tool()
    @workspace.set_tool(0, $objmap_select_tool.to_tool())
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
    if $use_worldmap
      tmpfile = "/tmp/tmpflexlay-worldmap.stwm"
      supertux_save_level(tmpfile)
    else
      # FIXME: use real tmpfile
      tmpfile = "/tmp/tmpflexlay-supertux.stl"
      supertux_save_level(tmpfile)
    end
    fork { exec("#{$datadir}/../supertux", tmpfile) }
  end

  def gui_resize_level()
    level = @workspace.get_map().get_metadata()
    dialog = $gui.create_generic_dialog("Resize Level")
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
    dialog = $gui.create_generic_dialog("Edit Level")

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
    dialog = $gui.create_generic_dialog("Edit Sector")
    
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
    gc = @editor_map.get_gc_state()
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

    name = "sector"
    uniq_name = name
    i = 2
    while level.get_sectors().index(uniq_name)
      uniq_name = name + "<%d>" % i
      i += 1                                    
    end

    sector = Sector.new(level)
    sector.new_from_size(uniq_name, 30, 20);
    level.add_sector(sector)
    level.activate_sector(uniq_name, @workspace)
    gui_edit_sector();
  end  

  def gui_switch_sector_menu()
    mymenu = Menu.new(CL_Point.new(530, 54), @gui.get_component())
    sector = @workspace.get_map().get_metadata()
    sector.parent.get_sectors().each { |i|
      if sector.name == i then
        current = " [current]"
      else
        current = ""
      end
      mymenu.add_item($mysprite, ("Sector (%s)%s" % [i, current]), proc { 
                        print "Switching to %s\n" % i
                        @workspace.get_map().get_metadata().parent.activate_sector(i, @workspace) 
                      })
    }

    mymenu.add_separator()
    mymenu.add_item($mysprite, "Create New Sector", proc {
        gui_add_sector()
        })
    mymenu.add_item($mysprite, "Remove Current Sector", proc {
        gui_remove_sector()
        })
    mymenu.add_item($mysprite, "Edit Sector Properties", proc {
        gui_edit_sector()
        })
    mymenu.run()
  end

  def gui_show_object_properties()
    $objmap_select_tool.get_selection()
    selection = $objmap_select_tool.get_selection()
    if selection.length() > 1 then
      print "Warning: Selection to large"
    elsif selection.length() == 1 then
      obj = selection[0].get_data()
      puts obj
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
    if $use_worldmap
      filename = @workspace.get_map().get_metadata().filename
    else
      filename = @workspace.get_map().get_metadata().parent.filename
    end
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

  def gui_level_new()
    puts "not implemented: gui_level_new()"
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
      obj = i.get_data()
      if obj.is_a?(PathNode)
        pathnodes.push(obj.node)
      end
    end

    last = nil
    for i in pathnodes
      if last != nil
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
    if map == nil || !map.instance_of?(Sector)
      return
    end
  
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
  if filename.scan(/.stwm$/) != []
    supertux_load_worldmap(filename)
    return
  end

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

def supertux_load_worldmap(filename)
  print "Loading: ", filename, "\n"
  worldmap = WorldMap.new(filename)
  worldmap.activate($gui.workspace)

  if not($recent_files.find{|el| el == filename}) then
    $recent_files.push(filename)
    $gui.recent_files_menu.add_item($mysprite, filename,
        proc { supertux_load_worldmap(filename) })
  end
  $gui.minimap.update_minimap()
  $use_worldmap = true
end

def supertux_save_level(filename)
  if $use_worldmap
    level = $gui.workspace.get_map().get_metadata()
  else
    level = $gui.workspace.get_map().get_metadata().parent
  end
  
  # Do backup save
  if File.exists?(filename) then
    File.rename(filename, filename + "~")
  end
  level.save(filename)
  level.filename = filename
end

# EOF #
