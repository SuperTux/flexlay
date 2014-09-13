# Flexlay - A Generic 2D Game Editor
# Copyright (C) 2014 Ingo Ruhnke <grumbel@gmail.com>
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.


from flexlay import Flexlay


class SuperTuxGUI
  quit_button = None
  menu        = None
  
  selector_window = None
  tileselector    = None
  objectselector  = None

  attr_reader :tileselector, :editor_map, :workspace, :minimap, :recent_files_menu, :gui

  def get_component(self):
    return self.gui.get_component()
  end

  def __init__(self):
    self.gui = $flexlay.create_gui_manager("SuperTux Editor")

    self.display_properties = DisplayProperties()

    self.editor_map = self.gui.create_editor_map_component()
    self.workspace = self.editor_map.get_workspace()

    self.workspace.set_tool(0, $tilemap_paint_tool.to_tool())
    self.workspace.set_tool(2, $workspace_move_tool.to_tool())
    self.workspace.set_tool(1, $tilemap_paint_tool.to_tool())
    # 'x' key
    self.workspace.set_tool(120, $zoom_tool.to_tool())
    # 'u' key
    self.workspace.set_tool(117, $objmap_select_tool.to_tool())

    self.workspace.set_tool(106, $workspace_move_tool.to_tool())
    self.workspace.set_tool(107, $zoom2_tool.to_tool())
    self.workspace.set_tool(65507, $zoom2_tool.to_tool())

    self.minimap = self.gui.create_minimap(self.editor_map)

    self.objectselector = self.gui.create_object_selector(42, 42)
    if False: # GRUMBEL
      connect_v2_ObjectBrush_Point(self.objectselector.sig_drop(), method(:on_object_drop))
    end
    $game_objects.each { |objectdata|
      sprite = load_cl_sprite($datadir + objectdata[1])
      self.objectselector.add_brush(ObjectBrush(sprite,
                                                make_metadata(objectdata)))
    }

    self.layer_selector = self.gui.create_layer_selector()

    self.tileselector = self.gui.create_tile_selector()
    self.tileselector.set_tileset($tileset)
    self.tileselector.set_tiles("All Tiles", $tileset.get_tiles())
    $tileset.tilegroups.each { |tilegroup|
      self.tileselector.set_tiles(tilegroup.name, tilegroup.tiles)
    }

    self.worldmapobjectselector = self.gui.create_object_selector(42, 42);
    if False:
      connect_v2_ObjectBrush_Point(self.worldmapobjectselector.sig_drop(),
                                   method(:on_worldmap_object_drop))
    end
    $worldmap_objects.each { |object|
      self.worldmapobjectselector.add_brush(ObjectBrush(
                                                        make_sprite($datadir + object[1]),
                                                        make_metadata(object[0])))
    }

    create_button_panel()

    self.toolbar = self.gui.create_button_panel(False)
    self.paint = self.toolbar.add_icon("../data/images/tools/stock-tool-pencil-22.png", proc{ set_tilemap_paint_tool() })
    self.select = self.toolbar.add_icon("../data/images/tools/stock-tool-rect-select-22.png", proc{ set_tilemap_select_tool() })
    self.toolbar.add_separator()
    self.object = self.toolbar.add_icon("../data/images/tools/stock-tool-clone-22.png", proc{ set_objmap_select_tool() })
    self.toolbar.add_separator()
    self.zoom = self.toolbar.add_icon("../data/images/tools/stock-tool-zoom-22.png", proc{ set_zoom_tool() })
    #     self.stroke = self.toolbar.add_icon("../data/images/tools/stock-tool-pencil-22.png", proc{ set_sketch_stroke_tool() })

    create_menu()

    self.load_dialog = self.gui.create_filedialog("Load SuperTux Level", "Load", "Cancel")
    self.load_dialog.set_filename($datadir + "levels/")
    self.save_dialog = self.gui.create_filedialog("Save SuperTux Level as...", "Save", "Cancel")
    self.save_dialog.set_filename($datadir + "levels/")

    register_keyboard_shortcuts()

    # Popup menu
    connect_v2($objmap_select_tool.sig_on_right_click(), proc{|x,y|
                 puts "Launching Menu at #{x}, #{y}"
                 menu = Menu(Point(x, y), self.gui.get_component())
                 menu.add_item($mysprite, "Delete Object(s)", proc{ 
                                 puts "Trying to delete #{self.workspace.get_map().get_metadata()} #{self.workspace.get_map().get_metadata().objects}"
                                 cmd = ObjectDeleteCommand(self.workspace.get_map().get_metadata().objects)
                                 $objmap_select_tool.get_selection().each { |i| cmd.add_object(i) }
                                 self.workspace.get_map().execute(cmd.to_command())
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

  def register_keyboard_shortcuts():
    if False: # GRUMBEL
      connect_v2(self.editor_map.sig_on_key("f1"), proc{ |x, y| gui_toggle_minimap()})
      connect_v2(self.editor_map.sig_on_key("m"),  proc{ |x, y| gui_toggle_minimap()})
      connect_v2(self.editor_map.sig_on_key("g"),  proc{ |x, y| gui_toggle_grid()})
      connect_v2(self.editor_map.sig_on_key("4"),  proc{ |x, y| gui_toggle_display_props()})

      connect_v2(self.editor_map.sig_on_key("3"),  proc{ |x, y| gui_show_foreground()})
      connect_v2(self.editor_map.sig_on_key("2"),  proc{ |x, y| gui_show_interactive()})
      connect_v2(self.editor_map.sig_on_key("1"),  proc{ |x, y| gui_show_background()})
      
      connect_v2(self.editor_map.sig_on_key("5"),  proc{ |x, y| self.editor_map.zoom_in(Point(x, y))})
      connect_v2(self.editor_map.sig_on_key("6"),  proc{ |x, y| self.editor_map.zoom_out(Point(x, y))})
      
      connect_v2(self.editor_map.sig_on_key("i"),  proc{ |x, y| insert_path_node(x,y)})
      connect_v2(self.editor_map.sig_on_key("c"),  proc{ |x, y| connect_path_nodes()})
      
      connect_v2(self.editor_map.sig_on_key("7"),  proc{ |x, y| self.workspace.get_map().get_metadata().parent.activate_sector("main", self.workspace)})
      connect_v2(self.editor_map.sig_on_key("8"),  proc{ |x, y| self.workspace.get_map().get_metadata().parent.activate_sector("another_world", self.workspace)})
      
      connect_v2(self.editor_map.sig_on_key("e"),  proc{ |x, y| gui_show_object_properties()})

      connect_v2(self.editor_map.sig_on_key("a"),  proc { |x, y|
                   pos = self.editor_map.screen2world(Point(x, y))
                   rectobj = ObjMapRectObject(Rect(pos,
                                                           Size(128, 64)),
                                                  Color(0, 255, 255, 155),
                                                  make_metadata(None))
                   # rectobj.set_metadata(metadata)
                   self.workspace.get_map().get_metadata().objects.add_object(rectobj.to_object())
                 })
    end
  end

  def create_menu(self):
    self.menubar = self.gui.create_menubar()

    file_menu = self.menubar.add_menu("&File")
    file_menu.add_item("New...", method(:gui_level_new))
    file_menu.add_item("Open...", method(:gui_level_load))
    self.recent_files_menu = file_menu.add_menu("Open Recent")
    file_menu.add_item("Save...", method(:gui_level_save))
    # file_menu.add_item("Save Commands...", menu_file_save_commands)
    # file_menu.add_item("Save As...", method(:gui_level_save_as))
    file_menu.add_item("Properties...", method(:gui_edit_level))
    file_menu.add_item("Quit",  proc{ self.gui.quit })
    
    edit_menu = self.menubar.add_menu("&Edit");
    edit_menu.add_item("Smooth Selection", method(:gui_smooth_level_struct))
    edit_menu.add_item("Resize", method(:gui_resize_level))
    edit_menu.add_item("Resize to selection", method(:gui_resize_level_to_selection))
    
    zoom_menu = self.menubar.add_menu("&Zoom");
    zoom_menu.add_item("1:4 (25%) ",  proc{ self.gui_set_zoom(0.25) })
    zoom_menu.add_item("1:2 (50%) ",  proc{ self.gui_set_zoom(0.5) })
    zoom_menu.add_item("1:1 (100%) ", proc{ self.gui_set_zoom(1.0) }) 
    zoom_menu.add_item("2:1 (200%) ", proc{ self.gui_set_zoom(2.0) })
    zoom_menu.add_item("4:1 (400%) ", proc{ self.gui_set_zoom(4.0) })

    layer_menu = self.menubar.add_menu("&Layer");
    layer_menu.add_item("Show all", proc{ gui_show_all() })
    layer_menu.add_item("Show current", proc{ gui_show_current() })
    layer_menu.add_item("Show only current", proc{ gui_show_only_current() })
  end

  def create_button_panel(self):
    button_panel = self.gui.create_button_panel(True)
    
    # File Handling
    button_panel.add_icon("../data/images/icons24/stock_new.png",  proc{ self.gui_level_new() })
    button_panel.add_icon("../data/images/icons24/stock_open.png", proc{ self.gui_level_load() })
    if False: # GRUMBEL
      button_panel.add_icon("../data/images/icons24/downarrow.png", proc{ self.recent_files_menu.run() })
      self.recent_files_menu = Menu(Point(32*2, 54), self.gui.get_component())
    end
    button_panel.add_icon("../data/images/icons24/stock_save.png", proc{ self.gui_level_save() })
    button_panel.add_icon("../data/images/icons24/stock_save_as.png", proc{ self.gui_level_save_as() })

    # Copy&Paste
    button_panel.add_separator()
    button_panel.add_icon("../data/images/icons24/stock_copy.png", proc{})
    button_panel.add_icon("../data/images/icons24/stock_paste.png", proc{})

    # Undo Redo
    button_panel.add_separator()
    self.undo_icon = button_panel.add_icon("../data/images/icons24/stock_undo.png", 
                                       proc{ self.workspace.get_map().undo() })
    self.redo_icon = button_panel.add_icon("../data/images/icons24/stock_redo.png", 
                                       proc{ self.workspace.get_map().redo() })

    self.undo_icon.disable()
    self.redo_icon.disable()

    # Visibility Toggles
    button_panel.add_separator()
    self.minimap_icon = button_panel.add_icon("../data/images/icons24/minimap.png", proc{ gui_toggle_minimap() })
    self.grid_icon    = button_panel.add_icon("../data/images/icons24/grid.png", proc{ gui_toggle_grid() })

    # Layers
    button_panel.add_separator()
    self.background_icon = button_panel.add_icon("../data/images/icons24/background.png", proc{ gui_show_background() })
    self.interactive_icon = button_panel.add_icon("../data/images/icons24/interactive.png", proc{ gui_show_interactive() })
    self.foreground_icon = button_panel.add_icon("../data/images/icons24/foreground.png", proc{ gui_show_foreground() })

    button_panel.add_separator()
    self.sector_icon = button_panel.add_icon("../data/images/icons24/sector.png", proc{ gui_switch_sector_menu() })

    button_panel.add_separator()
    self.run_icon = button_panel.add_icon("../data/images/icons24/run.png", proc{ gui_run_level() })

    self.tilegroup_icon = button_panel.add_icon("../data/images/icons24/eye.png", proc{ self.tilegroup_menu.run() })
  end

  def on_worldmap_object_drop(self, brush, pos):
    pos = self.editor_map.screen2world(pos)
    object_type = get_ruby_object(brush.get_data())
    create_worldmapobject_at_pos(
                                 $gui.workspace.get_map().get_metadata().objects, object_type, pos)
  end

  def on_object_drop(self, brush, pos):
    pos = self.editor_map.screen2world(pos)
    data = get_ruby_object(brush.get_data())
    create_gameobject($gui.workspace.get_map(), $gui.workspace.get_map().get_metadata().objects, data, pos)
  end

  def run(self):
    self.gui.run()
  end

  #   def show_colorpicker(self):
  #     self.tileselector.show(False)        
  #     self.objectselector.show(False)
  #     self.worldmapobjectselector.show(False)
  # #    self.colorpicker.show(True)
  #   end

  def show_objects(self):
    if False: # GRUMBEL
      self.tileselector.show(False)
      if $use_worldmap:
        self.worldmapobjectselector.show(True)
        self.objectselector.show(False)
      else:
        self.worldmapobjectselector.show(False)
        self.objectselector.show(True)
      end
      #    self.colorpicker.show(False)
    end
  end

  def show_tiles(self):
    if False: # GRUMBEL
      self.tileselector.show(True)        
      self.objectselector.show(False)
      self.worldmapobjectselector.show(False)
      # self.colorpicker.show(False)
    end
  end

  def show_none(self):
    if False: # GRUMBEL
      self.tileselector.show(False)        
      self.objectselector.show(False)
      self.worldmapobjectselector.show(False)
      # self.colorpicker.show(False)
    end
  end

  def set_tilemap_paint_tool(self):
    self.workspace.set_tool(0, $tilemap_paint_tool.to_tool())
    self.workspace.set_tool(1, $tilemap_paint_tool.to_tool())
    self.paint.set_down()
    self.select.set_up()
    self.zoom.set_up()
    self.object.set_up()
    show_tiles()
  end

  def set_tilemap_select_tool(self):
    self.workspace.set_tool(0, $tilemap_select_tool.to_tool())
    self.paint.set_up()
    self.select.set_down()
    self.zoom.set_up()
    self.object.set_up()
    show_none()
  end

  def set_zoom_tool(self):
    self.workspace.set_tool(0, $zoom_tool.to_tool())
    self.workspace.set_tool(1, $zoom_tool.to_tool())
    self.paint.set_up()
    self.select.set_up()
    self.zoom.set_down()
    self.object.set_up()
    show_none()
  end

  #   def set_sketch_stroke_tool(self):
  #     self.workspace.set_tool(0, $sketch_stroke_tool.to_tool())
  #     self.paint.set_up()
  #     self.select.set_up()
  #     self.zoom.set_up()
  #     self.object.set_down()
  #     show_colorpicker()
  #   end

  def set_objmap_select_tool(self):
    self.workspace.set_tool(0, $objmap_select_tool.to_tool())
    self.paint.set_up()
    self.select.set_up()
    self.zoom.set_up()
    self.object.set_down()
    show_objects()
  end

  def gui_show_foreground(self):
    self.display_properties.layer = FOREGROUND_LAYER
    self.display_properties.set(self.workspace.get_map().get_metadata())
    TilemapLayer.set_current(self.workspace.get_map().get_metadata().foreground)
    self.foreground_icon.set_down()
    self.interactive_icon.set_up()
    self.background_icon.set_up()
    self.minimap.update_minimap()
  end

  def gui_show_background(self):
    self.display_properties.layer = BACKGROUND_LAYER
    self.display_properties.set(self.workspace.get_map().get_metadata())
    TilemapLayer.set_current(self.workspace.get_map().get_metadata().background)
    self.foreground_icon.set_up()
    self.interactive_icon.set_up()
    self.background_icon.set_down()
    self.minimap.update_minimap()
  end

  def gui_show_interactive(self):
    puts "show_interactive"
    self.display_properties.layer = INTERACTIVE_LAYER
    self.display_properties.set(self.workspace.get_map().get_metadata())
    TilemapLayer.set_current(self.workspace.get_map().get_metadata().interactive)
    self.foreground_icon.set_up()
    self.interactive_icon.set_down()
    self.background_icon.set_up()
    self.minimap.update_minimap()
  end

  def gui_show_all(self):
    self.display_properties.show_all = True
    self.display_properties.current_only = False
    self.display_properties.set(self.workspace.get_map().get_metadata())
  end

  def gui_show_current(self):
    self.display_properties.show_all = False
    self.display_properties.current_only = False
    self.display_properties.set(self.workspace.get_map().get_metadata())
  end

  def gui_show_only_current(self):
    self.display_properties.show_all = False
    self.display_properties.current_only = True
    self.display_properties.set(self.workspace.get_map().get_metadata())
  end

  def gui_toggle_minimap(self):
    if False: # GRUMBEL
      if self.minimap.is_visible():
        self.minimap.show(False)
        self.minimap_icon.set_up()
      else:
        self.minimap.show(True)
        self.minimap_icon.set_down()
      end
    end
  end

  def gui_toggle_grid(self):
    tilemap = self.workspace.get_map().get_metadata().foreground;
    tilemap.set_draw_grid(!tilemap.get_draw_grid())
    
    if tilemap.get_draw_grid():
      self.grid_icon.set_down()
    else:
      self.grid_icon.set_up()
    end
  end
  
  def gui_toggle_display_props(self):
    if self.display_properties.show_all:
      self.display_properties.show_all = False
    elif not(self.display_properties.current_only):
      self.display_properties.current_only = True
    else:
      self.display_properties.show_all = True
      self.display_properties.current_only = False
    end
    
    self.display_properties.set(self.workspace.get_map().get_metadata())    
  end

  def gui_run_level(self):
    puts "Run this level..."
    if $use_worldmap:
      tmpfile = "/tmp/tmpflexlay-worldmap.stwm"
      supertux_save_level(tmpfile)
    else:
      # FIXME: use real tmpfile
      tmpfile = "/tmp/tmpflexlay-supertux.stl"
      supertux_save_level(tmpfile)
    end
    fork { exec("#{$datadir}/../supertux", tmpfile) }
  end

  def gui_resize_level(self):
    level = self.workspace.get_map().get_metadata()
    dialog = $gui.gui.create_generic_dialog("Resize Level")
    dialog.add_int("Width: ", level.width)
    dialog.add_int("Height: ", level.height)
    dialog.add_int("X: ", 0)
    dialog.add_int("Y: ", 0)
    dialog.set_callback(proc{|w, h, x, y| 
                          puts "Resize Callback"
                          level.resize(Size(w, h), Point(x, y))})
  end

  def gui_smooth_level_struct(self):
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
            and ($solid_itiles.index(get[x+1,y+1]) ? 1 : 0) == ary[8]):
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

  def gui_resize_level_to_selection(self):
    level = self.workspace.get_map().get_metadata()
    rect  = $tilemap_select_tool.get_selection_rect()
    if (rect.get_width() > 2 and rect.get_height() > 2):
      level.resize(rect.get_size(), Point(-rect.left, -rect.top))
    end
  end

  def gui_edit_level(self):
    level = self.workspace.get_map().get_metadata().get_level()
    dialog = $gui.gui.create_generic_dialog("Edit Level")

    dialog.add_string("Name:", level.name)
    dialog.add_string("Author:", level.author)
    # dialog.add_int("Time:", level.time)

    dialog.set_block() { |name, author|
      level.name   = name
      level.author = author
    }
  end

  def gui_edit_sector(self):
    level = self.workspace.get_map().get_metadata().get_level()
    dialog = $gui.gui.create_generic_dialog("Edit Sector")
    
    dialog.add_string("Name: ",   level.current_sector.name)
    dialog.add_string("Music: ",   level.current_sector.music)
    dialog.add_float("Gravity: ", level.current_sector.gravity)
    
    dialog.set_block() { |name, music, gravity|
      level.current_sector.name = name
      level.current_sector.music = music
      level.current_sector.gravity = gravity
    }
  end

  def gui_set_zoom(self, zoom):
    gc = self.editor_map.get_gc_state()
    pos = gc.get_pos()
    gc.set_zoom(zoom)
    gc.set_pos(pos)
  end

  def gui_remove_sector(self):
    sector = self.workspace.get_map().get_metadata()
    sector.get_level().remove_sector(sector.name)
  end

  def gui_add_sector(self):
    level = self.workspace.get_map().get_metadata().get_level()

    name = "sector"
    uniq_name = name
    i = 2
    while level.get_sectors().index(uniq_name)
      uniq_name = name + "<%d>" % i
      i += 1                                    
    end

    sector = Sector(level)
    sector.new_from_size(uniq_name, 30, 20);
    level.add_sector(sector)
    level.activate_sector(uniq_name, self.workspace)
    gui_edit_sector();
  end  

  def gui_switch_sector_menu(self):
    mymenu = Menu(Point(530, 54), self.gui.get_component())
    sector = self.workspace.get_map().get_metadata()
    sector.parent.get_sectors().each { |i|
      if sector.name == i:
        current = " [current]"
      else:
        current = ""
      end
      mymenu.add_item($mysprite, ("Sector (%s)%s" % [i, current]), proc { 
                        print "Switching to %s\n" % i
                        self.workspace.get_map().get_metadata().parent.activate_sector(i, self.workspace) 
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

  def gui_show_object_properties(self):
    $objmap_select_tool.get_selection()
    selection = $objmap_select_tool.get_selection()
    if len(selection) > 1:
      print "Warning: Selection to large"
    elif len(selection) == 1:
      obj = selection[0].get_data()
      puts obj
      obj.property_dialog()
    else:
      print "Warning: Selection is empty\n"

  def on_map_change(self):
    if self.workspace.get_map().undo_stack_size() > 0:
      self.undo_icon.enable()
    else:
      self.undo_icon.disable()
    end

    if self.workspace.get_map().redo_stack_size() > 0:
      self.redo_icon.enable()
    else:
      self.redo_icon.disable()        
    end
  end

  def gui_level_save_as(self):
    filename = self.save_dialog.get_filename()
    if filename[-1] == "/"[0]:
      self.save_dialog.set_filename(filename)
    else:
      self.save_dialog.set_filename(File.dirname(filename) + "/")
    end
    self.save_dialog.run(proc{|filename| supertux_save_level(filename) })
  end

  def gui_level_save(self):
    if $use_worldmap:
      filename = self.workspace.get_map().get_metadata().filename
    else:
      filename = self.workspace.get_map().get_metadata().parent.filename
    end
    print "Filename: ", filename, "\n"
    if filename:
      self.save_dialog.set_filename(filename)
    else:
      filename = self.save_dialog.get_filename()
      if filename[-1] == "/"[0]:
        self.save_dialog.set_filename(filename)
      else:
        self.save_dialog.set_filename(File.dirname(filename) + "/")
      end
    end
    
    self.save_dialog.run(proc{|filename| supertux_save_level(filename) })
  end   

  def gui_level_new(self):
    puts "not implemented: gui_level_new()"
  end

  def gui_level_load(self):
    self.load_dialog.run(proc{|filename| supertux_load_level(filename) })
  end

  def insert_path_node(self, x, y):
    print "Insert path Node"
    m = self.workspace.get_map().get_metadata()
    pathnode = ObjMapPathNode(self.editor_map.screen2world(Point(x, y)),
                                  make_metadata("PathNode"))
    pathnode.to_object().set_metadata(make_metadata(PathNode(pathnode)))
    m.objects.add_object(pathnode.to_object())
  end

  def connect_path_nodes(self):
    print "Connecting path nodes"
    pathnodes = []
    for i in $objmap_select_tool.get_selection()
      obj = i.get_data()
      if obj.is_a?(PathNode):
        pathnodes.push(obj.node)
      end
    end

    last = None
    for i in pathnodes
      if last != None:
        last.connect(i)
      end
      last = i
    end
  end
end

class DisplayProperties
  attr_reader :layer, :show_all, :current_only
  attr_writer :layer, :show_all, :current_only

  def __init__(self):
    self.layer        = INTERACTIVE_LAYER
    self.show_all     = False
    self.current_only = False
  end
  
  def set(self, map):
    puts map
    if map == None || !map.instance_of?(Sector):
      return
    end
    
    if self.current_only:
      active   = Color(255, 255, 255)
      deactive = Color(0, 0, 0, 10)
    else:
      active   = Color(255, 255, 255)
      deactive = Color(150, 150, 250, 150)
    end
    
    if (self.show_all):
      map.foreground.set_foreground_color(active)
      map.interactive.set_foreground_color(active)
      map.background.set_foreground_color(active)
    else:
      if (self.layer == FOREGROUND_LAYER):
        map.foreground.set_foreground_color(active)
      else:
        map.foreground.set_foreground_color(deactive)
      end
      
      if (self.layer == INTERACTIVE_LAYER):
        map.interactive.set_foreground_color(active)
      else:
        map.interactive.set_foreground_color(deactive)
      end
      
      if (self.layer == BACKGROUND_LAYER):
        map.background.set_foreground_color(active)
      else:
        map.background.set_foreground_color(deactive)
      end
    end
  end
end


def supertux_load_level(filename)
  if filename.scan(/.stwm$/) != []:
    supertux_load_worldmap(filename)
    return
  end

  print "Loading: ", filename, "\n"
  level = Level(filename)
  level.activate($gui.workspace)
  
  if not($recent_files.find{|el| el == filename}):
    $recent_files.push(filename)
    $gui.recent_files_menu.add_item(filename, 
                                    proc { supertux_load_level(filename) })
  end
  
  $gui.minimap.update_minimap()
end

def supertux_load_worldmap(filename)
  print "Loading: ", filename, "\n"
  worldmap = WorldMap(filename)
  worldmap.activate($gui.workspace)

  if not($recent_files.find{|el| el == filename}):
    $recent_files.push(filename)
    $gui.recent_files_menu.add_item(filename, 
                                    proc { supertux_load_worldmap(filename) })
  end
  $gui.minimap.update_minimap()
  $use_worldmap = True
end

def supertux_save_level(filename)
  if $use_worldmap:
    level = $gui.workspace.get_map().get_metadata()
  else:
    level = $gui.workspace.get_map().get_metadata().parent
  end
  
  # Do backup save
  if File.exists?(filename):
    File.rename(filename, filename + "~")
  end
  level.save(filename)
  level.filename = filename
end

# EOF #
