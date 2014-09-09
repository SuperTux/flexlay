# Flexlay - A Generic 2D Game Editor
# Copyright (C) 2002 Ingo Ruhnke <grumbel@gmail.com>
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.

class GUI
  attr_reader :workspace, :minimap, :editor_map

  def initialize()
    @gui = GUIManager.new()

    @editor_map = @gui.create_editor_map_component()
    @workspace = @editor_map.get_workspace()

    @brushbox = @gui.create_tile_brush_selector()
    if false # GRUMBEL
      connect_v1_cl(@brushbox.sig_highlighted(), method(:brushbox_change))
    end

    @objectselector = @gui.create_object_selector(42, 42)

    @objectselector.add_brush(ObjectBrush.new(GameObjects::Outpost.get_sprite(),
                                              make_metadata(proc{GameObjects::Outpost.new()})))
    @objectselector.add_brush(ObjectBrush.new(GameObjects::SpawnPoint.get_sprite(),
                                              make_metadata(proc{GameObjects::SpawnPoint.new()})))

    $brushes.size.times {|i|
      @objectselector.add_brush(ObjectBrush.new(make_sprite($config.datadir + "/thumbnails/#{i}.png"),
                                                make_metadata(proc{GameObjects::TileObject.new(i)})))
    }
    connect_v2_ObjectBrush_Point(@objectselector.sig_drop(), method(:on_object_drop))
    
    @workspace.set_tool(0, $tilemap_paint_tool.to_tool());
    @workspace.set_tool(2, $workspace_move_tool.to_tool());

    @button_panel = @gui.create_button_panel(true)

    @button_panel.add_icon($flexlay_datadir + "/images/icons24/stock_new.png",
                           proc{ gui_level_new() })
    @button_panel.add_icon($flexlay_datadir + "/images/icons24/stock_open.png", 
                           proc{ gui_level_load() })
    @button_panel.add_icon($flexlay_datadir + "/images/icons24/downarrow.png", 
                           proc{ gui_level_load() })
    @button_panel.add_icon($flexlay_datadir + "/images/icons24/stock_save.png",
                           proc{ gui_level_save() })
    @button_panel.add_icon($flexlay_datadir + "/images/icons24/stock_save_as.png", 
                           proc{ gui_level_save_as() })

    @button_panel.add_separator()
    @button_panel.add_icon($flexlay_datadir + "/images/icons24/stock_copy.png", proc{})
    @button_panel.add_icon($flexlay_datadir + "/images/icons24/stock_paste.png", proc{})
    @button_panel.add_separator()
    @undo_icon = @button_panel.add_icon($flexlay_datadir + "/images/icons24/stock_undo.png", proc{@workspace.get_map().undo()})
    @redo_icon = @button_panel.add_icon($flexlay_datadir + "/images/icons24/stock_redo.png", proc{@workspace.get_map().redo()})
    @button_panel.add_separator()


    @tool_button_panel = @gui.create_button_panel(true)
    @tool_button_panel.add_separator()
    @tool_button_panel.add_icon($flexlay_datadir + "/images/icons24/object_raise.png", proc{
                                  $objmap_select_tool.get_selection().each {|obj|
                                    @workspace.get_map().get_data().objects.raise(obj)
                                  }
                                })
    @tool_button_panel.add_icon($flexlay_datadir + "/images/icons24/object_lower.png", proc{
                                  $objmap_select_tool.get_selection().each {|obj|
                                    @workspace.get_map().get_data().objects.lower(obj)
                                  }
                                })
    @tool_button_panel.show(false)


    @toolbar = @gui.create_button_panel(false)
    @paint = @toolbar.add_icon($flexlay_datadir + "/images/tools/stock-tool-pencil-22.png",
                               method(:set_tilemap_paint_tool))
    @select = @toolbar.add_icon($flexlay_datadir + "/images/tools/stock-tool-rect-select-22.png",
                                method(:set_tilemap_select_tool))
    @zoom = @toolbar.add_icon($flexlay_datadir + "/images/tools/stock-tool-zoom-22.png",
                              method(:set_zoom_tool))
    @object = @toolbar.add_icon($flexlay_datadir + "/images/tools/stock-tool-clone-22.png",
                                method(:set_objmap_select_tool))

    $brushes.each {|i|
      (index, width, height, name) = i
      
      brush = TileBrush.new(width, height)
      brush.set_data(Range.new(index, index + (width*height)-1).to_a)

      @brushbox.add_brush("%s - %sx%s" % [name, width, height], brush)
    }

    @menubar = @gui.create_menubar()
    file_menu = @menubar.add_menu("File")
    file_menu.add_item("New...", proc{gui_level_new})
    file_menu.add_item("Open...", proc{gui_level_load})
    file_menu.add_item("Save...", proc{gui_level_save})
    file_menu.add_item("Save As...", proc{gui_level_save_as})
    file_menu.add_item("Quit",  proc{@gui.quit})

    zoom_menu = @menubar.add_menu("Zoom")
    zoom_menu.add_item("1:4 (25%) ",  proc{ gui_set_zoom(0.25) })
    zoom_menu.add_item("1:2 (50%) ",  proc{ gui_set_zoom(0.5) })
    zoom_menu.add_item("1:1 (100%) ", proc{ gui_set_zoom(1.0) })
    zoom_menu.add_item("2:1 (200%) ", proc{ gui_set_zoom(2.0) })
    zoom_menu.add_item("4:1 (400%) ", proc{ gui_set_zoom(4.0) })

    script_menu = @menubar.add_menu("Scripts")
    script_menu.add_item("Flatten",  proc{ @workspace.get_map().get_data().flatten() })
    script_menu.add_item("Unflatten",  proc{ @workspace.get_map().get_data().unflatten() })

    @minimap = @gui.create_minimap(@editor_map)

    @load_dialog = @gui.create_filedialog("Load netPanzer Level", "Load", "Cancel")
    @load_dialog.set_filename($config.datadir + "/maps/")
    @save_dialog = @gui.create_filedialog("Save netPanzer Level as...", "Save", "Cancel")
    @save_dialog.set_filename($config.datadir + "/maps/")

    if false # GRUMBEL
      connect_v2(@editor_map.sig_on_key("l"), proc{ |x, y|
                   $objmap_select_tool.get_selection().each {|obj|
                     @workspace.get_map().get_data().objects.raise(obj)
                   }
                 })
      connect_v2(@editor_map.sig_on_key("s"), proc{ |x, y| 
                   $objmap_select_tool.get_selection().each {|obj|
                     @workspace.get_map().get_data().objects.lower(obj)
                   }
                 })

      connect_v2($objmap_select_tool.sig_on_right_click(), proc{|x,y|
                   puts "Launching Menu at #{x}, #{y}"
                   menu = Menu.new(Point.new(x-16, y-16), @gui.get_component())
                   menu.add_item("Delete Selection", proc{ 
                                   cmd = ObjectDeleteCommand.new(@workspace.get_map().get_metadata().objects)
                                   $objmap_select_tool.get_selection().each { |i| cmd.add_object(i) }
                                   @workspace.get_map().execute(cmd.to_command())
                                   $objmap_select_tool.clear_selection()
                                 })
                   menu.add_item("Flatten Selection", proc{
                                   @workspace.get_map().get_data().objects.get_objects().each{|obj|
                                     obj.get_data().draw_to_tilemap(@workspace.get_map().get_data().tilemap)
                                   }
                                   cmd = ObjectDeleteCommand.new(@workspace.get_map().get_metadata().objects)
                                   @workspace.get_map().execute(cmd.to_command())
                                   $objmap_select_tool.get_selection().each { |i| cmd.add_object(i) }
                                   @workspace.get_map().execute(cmd.to_command())
                                   $objmap_select_tool.clear_selection()
                                 })
                   menu.add_separator()
                   menu.add_item(make_sprite($flexlay_datadir + "/images/icons16/object_raise.png"), 
                                 "Raise Selection", proc{
                                   $objmap_select_tool.get_selection().each {|obj|
                                     @workspace.get_map().get_data().objects.raise(obj)
                                   }
                                 })
                   menu.add_item(make_sprite($flexlay_datadir + "/images/icons16/object_lower.png"), 
                                 "Lower Selection", proc{
                                   $objmap_select_tool.get_selection().each {|obj|
                                     @workspace.get_map().get_data().objects.lower(obj)
                                   }
                                 })
                   menu.run()
                 })
    end
  end

  def brushbox_change(index)
    (start, width,  height, name) = $brushes[index]
    brush = TileBrush.new(width, height)
    brush.set_data(Range.new(start, start + width*height).to_a)
    $tilemap_paint_tool.set_brush(brush)
  end

  def on_object_drop(brush, pos)
    obj = get_ruby_object(brush.get_data()).call()
    pos = @editor_map.screen2world(pos)
    puts "<>>>>>>>>< ", obj.get_sprite(), pos
    sprite_obj = ObjMapSpriteObject.new(obj.get_sprite(), pos, make_metadata(obj))
    obj.data = sprite_obj
    
    cmd = ObjectAddCommand.new(@workspace.get_map().get_data().objects)
    cmd.add_object(sprite_obj.to_object)
    @workspace.get_map().execute(cmd.to_command())
  end

  def set_tilemap_paint_tool()
    @workspace.set_tool(0, $tilemap_paint_tool.to_tool())
    @paint.set_down()
    @select.set_up()
    @zoom.set_up()
    @object.set_up()

    @brushbox.show(true)
    @objectselector.show(false)

    @tool_button_panel.show(false)
  end

  def set_tilemap_select_tool()
    @workspace.set_tool(0, $tilemap_select_tool.to_tool())
    @paint.set_up()
    @select.set_down()
    @zoom.set_up()
    @object.set_up()

    @brushbox.show(false)
    @objectselector.show(false)

    @tool_button_panel.show(false)
  end

  def set_zoom_tool()
    @workspace.set_tool(0, $zoom_tool.to_tool())
    @paint.set_up()
    @select.set_up()
    @zoom.set_down()
    @object.set_up()

    @brushbox.show(false)
    @objectselector.show(false)

    @tool_button_panel.show(false)
  end

  def set_objmap_select_tool()
    @workspace.set_tool(0, $objmap_select_tool.to_tool())
    @paint.set_up()
    @select.set_up()
    @zoom.set_up()
    @object.set_down()

    @brushbox.show(false)
    @objectselector.show(true)

    @tool_button_panel.show(true)
  end


  def gui_level_save()
    if @workspace.get_map().get_metadata().filename
      @save_dialog.set_filename(@workspace.get_map().get_metadata().filename)
    else
      @save_dialog.set_filename(File::dirname(@save_dialog.get_filename())  + "/")
    end
    
    @save_dialog.run(method(:netpanzer_save_level))
  end

  def gui_level_load()
    @load_dialog.run(method(:netpanzer_load_level))
  end


  def gui_level_new()
    dialog = @gui.create_generic_dialog("Create New Level")
    dialog.add_string("Name: ", "New Level")
    dialog.add_int("Width: ", 128)
    dialog.add_int("Height: ", 128)
    dialog.set_callback(proc{|name, width, height|
                          level = Level.new(width, height)
                          level.activate(@workspace)
                          level.name = name
                          connect(level.editormap.sig_change(), method(:on_map_change))
                        })
  end

  def gui_set_zoom(zoom)
    gc = @editor_map.get_gc_state()
    pos = gc.get_pos()
    gc.set_zoom(zoom)
    gc.set_pos(pos)
  end

  def gui_level_save_as()
    @save_dialog.set_filename(File::dirname(@save_dialog.get_filename()) + "/")
    @save_dialog.run(method(:netpanzer_save_level))
  end

  def gui_toggle_grid()
    tilemap = $workspace.get_map().get_metadata().data.get_tilemap()

    #FIXMEgrid_status = not(tilemap.get_draw_grid())
    #tilemap.set_draw_grid(grid_status)

    if tilemap.get_draw_grid() then
      grid_icon.set_down()
    else
      grid_icon.set_up()
      
      grid_icon = Icon(Rect(Point(p.inc(48), 2), Size(32, 32)),
                       make_sprite($flexlay_datadir + "/images/icons24/grid.png"), "Some tooltip", button_panel);
      grid_icon.set_callback(proc{gui_toggle_grid})

      layer_menu = Menu(Point(32*11+2, 54), $gui.get_component())
    end
  end

  def netpanzer_new_level(w, h, name)
    level = Level.new(w, h)
    level.activate(@workspace)
    level.name = name
    connect(level.editormap.sig_change(), method(:on_map_change))
  end

  def netpanzer_load_level(filename)
    level = Level.new(filename)
    level.activate($gui.workspace)
    connect(level.editormap.sig_change(), method(:on_map_change))
    
    #  if not(has_element($config.recent_files, filename))
    #    $config.recent_files.push(filename)
    #    recent_files_menu.add_item(mysprite, filename, proc{ netpanzer_load_level(filename) })
    #  end
    $gui.minimap.update_minimap()
  end

  def netpanzer_save_level(filename)
    $gui.workspace.get_map().get_data().save(filename)
  end

  # recent_files_menu = Menu.new(Point.new(32*2, 54), $gui.get_component())
  # for filename in $config.recent_files
  #    recent_files_menu.add_item(mysprite, filename, proc{ netpanzer_load_level(filename) })
  #end

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

  def run()
    @gui.run()
  end  
end

# EOF #
