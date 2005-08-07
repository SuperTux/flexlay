##  $Id$
## 
##  Flexlay - A Generic 2D Game Editor
##  Copyright (C) 2004 Ingo Ruhnke <grumbel@gmx.de>
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

## Create some Basic GUI, this is a bit more complicated then it
## should be due to the lack of proper button-banel class and layout manager

$screen_width  = 1024
$screen_height = 768

## GUI class which holds all the GUI components and the state of them
class GUI
  attr_reader :workspace, :gui, :tileselector, :objectselector

  def run()
    ## Enter main loop here
    @gui.run()
  end

  def initialize()
    @gui = GUIManager.new()

    myrect      = CL_Rect.new(CL_Point.new(0, 56), CL_Size.new($screen_width - 800 +  665,
                                                               $screen_height - 600 + 488+56))
    @editor_map = EditorMapComponent.new(myrect, @gui.get_component())
    @workspace  = Workspace.new(myrect.get_width(), myrect.get_height())
    @editor_map.set_workspace(@workspace)

    @minimap = Minimap.new(@editor_map, CL_Rect.new(CL_Point.new(3 + myrect.left, 
                                                                 $screen_height - 600 + 488+3-14  + myrect.top), 
                                                    CL_Size.new($screen_width - 6 -134-16, 50)), 
                           @gui.get_component())

    @button_panel = ButtonPanel.new(0, 23, $screen_width, 33, true, @gui.get_component)

    @button_panel.add_icon("../data/images/icons24/stock_new.png")
    @button_panel.add_icon("../data/images/icons24/stock_open.png", proc{ level_load() })
    @button_panel.add_small_icon("../data/images/icons24/downarrow.png", proc{ $controller.recent_files_menu.run() })
    @button_panel.add_icon("../data/images/icons24/stock_save.png", proc{ level_save() })
    @button_panel.add_icon("../data/images/icons24/stock_save_as.png", proc{ level_save_as() })
    @button_panel.add_separator()
    @button_panel.add_icon("../data/images/icons24/stock_copy.png")
    @button_panel.add_icon("../data/images/icons24/stock_paste.png")
    @button_panel.add_separator()
    @undo_icon = @button_panel.add_icon("../data/images/icons24/stock_undo.png", proc{ @workspace.get_map().undo() })
    @redo_icon = @button_panel.add_icon("../data/images/icons24/stock_redo.png", proc{ @workspace.get_map().redo() })
    @undo_icon.disable()
    @redo_icon.disable()
    @button_panel.add_separator()
    @minimap_icon = @button_panel.add_icon("../data/images/icons24/minimap.png", proc{ toggle_minimap() })
    @grid_icon = @button_panel.add_icon("../data/images/icons24/grid.png", proc{ toggle_grid() })
    @button_panel.add_separator()

    @background_icon = @button_panel.add_icon("../data/images/icons24/background.png")
    @interactive_icon = @button_panel.add_icon("../data/images/icons24/interactive.png")
    @foreground_icon = @button_panel.add_icon("../data/images/icons24/foreground.png")
    @eye_icon = @button_panel.add_icon("../data/images/icons24/eye.png")

    @button_panel.add_icon("../data/images/icons24/eye.png", proc{ @tilegroup_menu.run() })

    @layer_menu = Menu.new(CL_Point.new(32*15+2, 54), @gui.get_component())
    
    @toolbar = ButtonPanel.new(0, 23+33, 33, 32*4+2, false, @gui.get_component)
    @paint  = @toolbar.add_icon("../data/images/tools/stock-tool-pencil-22.png", proc{ $controller.set_tilemap_paint_tool() })
    @select = @toolbar.add_icon("../data/images/tools/stock-tool-rect-select-22.png", proc{ $controller.set_tilemap_select_tool() })
    @zoom   = @toolbar.add_icon("../data/images/tools/stock-tool-zoom-22.png", proc{ $controller.set_zoom_tool() })
    @object = @toolbar.add_icon("../data/images/tools/stock-tool-clone-22.png", proc{ $controller.set_objmap_select_tool() })

    @foreground_icon.set_callback(proc{ show_foreground() })
    @interactive_icon.set_callback(proc{ show_interactive() })
    @background_icon.set_callback(proc{ show_background() })
    @eye_icon.set_callback(proc{ $layer_menu.run() })

    @layer_menu.add_item($mysprite, "Show all", proc{ show_all() })
    @layer_menu.add_item($mysprite, "Show current", proc{ show_current() })
    @layer_menu.add_item($mysprite, "Show only current", proc{ show_only_current() })

    @menu = CL_Menu.new(@gui.get_component())
    @menu.add_item("File/Open...", proc{ level_load() })
    @menu.add_item("File/Save...", proc{ level_save() })
    # @menu.add_item("File/Save Commands...", menu_file_save_commands)
    @menu.add_item("File/Save As...", proc{ level_save_as() })
    @menu.add_item("File/Quit",  proc{ @gui.quit() })

    @menu.add_item("Edit/Resize", proc{ resize_level() })
    @menu.add_item("Edit/Resize to selection", proc{ resize_level_to_selection()})
    @menu.add_item("Edit/Debug Shell", proc{ run_python()})

    @menu.add_item("Zoom/1:4 (25%) ",  proc{ set_zoom(0.25) })
    @menu.add_item("Zoom/1:2 (50%) ",  proc{ set_zoom(0.5) })
    @menu.add_item("Zoom/1:1 (100%) ", proc{ set_zoom(1.0) }) 
    @menu.add_item("Zoom/2:1 (200%) ", proc{ set_zoom(2.0) })
    @menu.add_item("Zoom/4:1 (400%) ", proc{ set_zoom(4.0) })

    @load_dialog = SimpleFileDialog.new("Load SuperTux Level", "Load", "Cancel", @gui.get_component())
    @load_dialog.set_filename($datadir + "levels/")
    @save_dialog = SimpleFileDialog.new("Save SuperTux Level as...", "Save", "Cancel", @gui.get_component())
    @save_dialog.set_filename($datadir + "levels/")

    # FIXME: Having position in the Menus here is EXTREMLY ugly
    @tilegroup_menu = Menu.new(CL_Point.new(35*15+2, 54), @gui.get_component())
    @tilegroup_menu.add_item($mysprite, "All Tiles", proc{@tileselector.set_tiles($tileset.get_tiles())})
    $tileset.tilegroups.each { |tilegroup|
      @tilegroup_menu.add_item($mysprite, tilegroup.name, proc{@tileselector.set_tiles(tilegroup.tiles)})
    }

    toggle_minimap()

    # Init the GUI, so that button state is in sync with internal state
    # show_interactive()
    # show_current()
    # set_tilemap_paint_tool()

    @selector_window = Panel.new(CL_Rect.new(CL_Point.new($screen_width-128-64-6, 23+33), 
                                             CL_Size.new(128 + 64 + 6, $screen_height - 600 + 558)),
                                 @gui.get_component())
    @tileselector = TileSelector.new(CL_Rect.new(CL_Point.new(3, 3), 
                                                 CL_Size.new(128+64, $screen_height - 600 + 552)),
                                     @selector_window)
    @tileselector.set_tileset($tileset)
    @tileselector.set_scale(0.375)
    # @tileselector.set_tiles($tileset.tilegroups[0].tiles)
    @tileselector.set_tiles($tileset.get_tiles())
    @tileselector.show(true)
    
    @objectselector = ObjectSelector.new(CL_Rect.new(0, 0, 128, $screen_height - 600 + 552), 42, 42, @selector_window)
    @objectselector.show(false)

    connect_v2_ObjectBrush_Point(@objectselector.sig_drop(), proc{|brush, point| on_object_drop() })

#    $game_objects.each do |object|
#     @objectselector.add_brush(ObjectBrush.new(make_sprite($datadir + object[1]),
#                                                make_metadata(object)))
#    end

    @load_dialog = SimpleFileDialog.new("Load SuperTux Level", "Load", "Cancel", @gui.get_component())
    @load_dialog.set_filename($datadir + "levels/")
    @save_dialog = SimpleFileDialog.new("Save SuperTux Level as...", "Save", "Cancel", @gui.get_component())
    @save_dialog.set_filename($datadir + "levels/")

    connect_v2(@editor_map.sig_on_key("f1"), proc{ |x, y| toggle_minimap()})
    connect_v2(@editor_map.sig_on_key("m"),  proc{ |x, y| toggle_minimap()})
    connect_v2(@editor_map.sig_on_key("g"),  proc{ |x, y| toggle_grid()})
    connect_v2(@editor_map.sig_on_key("4"),  proc{ |x, y| toggle_display_props()})
    connect_v2(@editor_map.sig_on_key("3"),  proc{ |x, y| show_foreground()})
    connect_v2(@editor_map.sig_on_key("2"),  proc{ |x, y| show_interactive()})
    connect_v2(@editor_map.sig_on_key("1"),  proc{ |x, y| show_background()})
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

  def show_background()
    TilemapLayer.set_current(@workspace.get_map().get_metadata().background)
  end

  def show_interactive()
    TilemapLayer.set_current(@workspace.get_map().get_metadata().interactive)
  end

  def show_foreground()
    TilemapLayer.set_current(@workspace.get_map().get_metadata().foreground)
  end

  def level_load()
    @load_dialog.run(proc{|filename| $controller.load_level(filename) })
  end

  def level_save()
    @save_dialog.run(proc{|filename| $controller.save_level(filename) })
  end

  def level_save_as()
    @save_dialog.run(proc{|filename| $controller.save_level(filename) })
  end
  
  def toggle_minimap()
    if @minimap.is_visible()
      @minimap.show(false)
      @minimap_icon.set_up()
    else
      @minimap.show(true)
      @minimap_icon.set_down()
    end
  end

  def toggle_grid()
    tilemap = @workspace.get_map().get_data().layers[0];
    tilemap.set_draw_grid(!tilemap.get_draw_grid())

    if tilemap.get_draw_grid()
      @grid_icon.set_down()
    else
      @grid_icon.set_up()
    end
  end

  def set_tilemap_paint_tool()
    @paint.set_down()
    @select.set_up()
    @zoom.set_up()
    @object.set_up()
  end

  def set_tilemap_select_tool()
    @paint.set_up()
    @select.set_down()
    @zoom.set_up()
    @object.set_up()
  end

  def set_zoom_tool()
    @paint.set_up()
    @select.set_up()
    @zoom.set_down()
    @object.set_up()
  end

  def set_objmap_select_tool()
    @paint.set_up()
    @select.set_up()
    @zoom.set_up()
    @object.set_down()
  end

  def set_zoom(zoom)
    gc = @editor_map.get_workspace().get_gc_state()
    pos = gc.get_pos()
    gc.set_zoom(zoom)
    gc.set_pos(pos)
  end
end

# EOF #
