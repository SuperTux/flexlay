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

## GUI class which holds all the GUI components and the state of them
class GUI
  attr_reader :workspace

  def run()
    ## Enter main loop here
    @gui.run()
  end

  def initialize()
    ## Init the GUI manager
    @editor = Editor.new()
    @gui = @editor.get_gui_manager()

    myrect      = CL_Rect.new(CL_Point.new(0, 56), CL_Size.new(665, 488+56))
    @editor_map = EditorMapComponent.new(myrect, @gui.get_component())
    @workspace  = Workspace.new(myrect.get_width(), myrect.get_height())
    @editor_map.set_workspace(@workspace)

    @minimap = Minimap.new($editor_map, CL_Rect.new(CL_Point.new(3 + myrect.left, 
                                                                 488+3-14  + myrect.top), 
                                                    CL_Size.new(794-134-16, 50)), 
                           @gui.get_component())

    @button_panel = Panel.new(CL_Rect.new(CL_Point.new(0, 23), CL_Size.new(800, 33)), @gui.get_component())


    p = Counter.new(2)

    @new_icon = Icon.new(CL_Rect.new(CL_Point.new(p.inc(0),  2), CL_Size.new(32, 32)),
                         make_sprite("../data/images/icons24/stock_new.png"), "Some tooltip", @button_panel);
    @load_icon = Icon.new(CL_Rect.new(CL_Point.new(p.inc(32), 2), CL_Size.new(32, 32)),
                          make_sprite("../data/images/icons24/stock_open.png"), "Some tooltip", @button_panel);
    @load_recent_icon = Icon.new(CL_Rect.new(CL_Point.new(p.inc(32), 2), CL_Size.new(16, 32)),
                                 make_sprite("../data/images/icons24/downarrow.png"), "Some tooltip", @button_panel);
    @save_icon        = Icon.new(CL_Rect.new(CL_Point.new(p.inc(16), 2), CL_Size.new(32, 32)),
                                 make_sprite("../data/images/icons24/stock_save.png"), "Some tooltip", @button_panel);
    @save_as_icon     = Icon.new(CL_Rect.new(CL_Point.new(p.inc(32), 2), CL_Size.new(32, 32)),
                                 make_sprite("../data/images/icons24/stock_save_as.png"), "Some tooltip", @button_panel);

    @load_icon.set_callback(proc{ level_load() })
    @load_recent_icon.set_callback(proc{ $recent_files_menu.run() })
    @save_icon.set_callback(proc{ level_save() })
    @save_as_icon.set_callback(proc{ level_save_as() })

    @copy_icon    = Icon.new(CL_Rect.new(CL_Point.new(p.inc(48), 2), CL_Size.new(32, 32)),
                             make_sprite("../data/images/icons24/stock_copy.png"), "Some tooltip", @button_panel);
    @paste_icon   = Icon.new(CL_Rect.new(CL_Point.new(p.inc(32), 2), CL_Size.new(32, 32)),
                             make_sprite("../data/images/icons24/stock_paste.png"), "Some tooltip", @button_panel);

    @undo_icon = Icon.new(CL_Rect.new(CL_Point.new(p.inc(48), 2), CL_Size.new(32, 32)),
                          make_sprite("../data/images/icons24/stock_undo.png"), "Some tooltip", @button_panel);
    @redo_icon = Icon.new(CL_Rect.new(CL_Point.new(p.inc(32), 2), CL_Size.new(32, 32)),
                          make_sprite("../data/images/icons24/stock_redo.png"), "Some tooltip", @button_panel);

    @undo_icon.set_callback(proc{ @workspace.get_map().undo() })
    @redo_icon.set_callback(proc{ @workspace.get_map().redo() })

    @undo_icon.disable()
    @redo_icon.disable()

    @minimap_icon = Icon.new(CL_Rect.new(CL_Point.new(p.inc(48), 2), CL_Size.new(32, 32)),
                             make_sprite("../data/images/icons24/minimap.png"), "Some tooltip", @button_panel);
    @minimap_icon.set_callback(proc{ toggle_minimap() })

    @grid_icon = Icon.new(CL_Rect.new(CL_Point.new(p.inc(32), 2), CL_Size.new(32, 32)),
                          make_sprite("../data/images/icons24/grid.png"), "Some tooltip", @button_panel);
    @grid_icon.set_callback(proc{ toggle_grid() })

    @background_icon  = Icon.new(CL_Rect.new(CL_Point.new(p.inc(48), 2), CL_Size.new(32, 32)),
                                 make_sprite("../data/images/icons24/background.png"), "Some tooltip", @button_panel);
    @interactive_icon = Icon.new(CL_Rect.new(CL_Point.new(p.inc(32), 2), CL_Size.new(32, 32)),
                                 make_sprite("../data/images/icons24/interactive.png"), "Some tooltip", @button_panel);
    @foreground_icon  = Icon.new(CL_Rect.new(CL_Point.new(p.inc(32), 2), CL_Size.new(32, 32)),
                                 make_sprite("../data/images/icons24/foreground.png"), "Some tooltip", @button_panel);
    @eye_icon         = Icon.new(CL_Rect.new(CL_Point.new(p.inc(32), 2), CL_Size.new(32, 32)),
                                 make_sprite("../data/images/icons24/eye.png"), "Some tooltip", @button_panel);

    @layer_menu = Menu.new(CL_Point.new(32*15+2, 54), @gui.get_component())

    @toolbar = Panel.new(CL_Rect.new(CL_Point.new(0, 23+33), CL_Size.new(33, 32*4+2)), @gui.get_component())

    @paint = Icon.new(CL_Rect.new(CL_Point.new(2, 32*0+2), CL_Size.new(32, 32)), make_sprite("../data/images/tools/stock-tool-pencil-22.png"), "Some tooltip", @toolbar);
    @paint.set_callback(proc{ $controller.set_tilemap_paint_tool() })

    @select = Icon.new(CL_Rect.new(CL_Point.new(2, 32*1+2), CL_Size.new(32,32)), make_sprite("../data/images/tools/stock-tool-rect-select-22.png"), "Some tooltip", @toolbar);
    @select.set_callback(proc{ $controller.set_tilemap_select_tool() })

    @zoom = Icon.new(CL_Rect.new(CL_Point.new(2, 32*2+2), CL_Size.new(32,32)), make_sprite("../data/images/tools/stock-tool-zoom-22.png"), "Some tooltip", @toolbar);
    @zoom.set_callback(proc{ $controller.set_zoom_tool() })

    @object = Icon.new(CL_Rect.new(CL_Point.new(2, 32*3+2), CL_Size.new(32,32)), make_sprite("../data/images/tools/stock-tool-clone-22.png"), "Some tooltip", @toolbar);
    @object.set_callback(proc{ $controller.set_objmap_select_tool() })

    # erase  = Icon.new(CL_Point.new(2, 32+1+2), make_sprite("../data/images/tools/stock-tool-eraser-22.png"), "Some tooltip", $toolbar);
    # move   = Icon.new(CL_Point.new(2, 32*2+2), make_sprite("../data/images/tools/stock-tool-move-22.png"), "Some tooltip", $toolbar);

    # $foreground_icon.set_callback(proc{ show_foreground() })
    # $interactive_icon.set_callback(proc{ show_interactive() })
    # $background_icon.set_callback(proc{ show_background() })
    # $eye_icon.set_callback(proc{ $layer_menu.run() })

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

    toggle_minimap()

    # Init the GUI, so that button state is in sync with internal state
    # show_interactive()
    # show_current()
    # set_tilemap_paint_tool()

    connect_v2(@editor_map.sig_on_key("f1"), proc{ |x, y| toggle_minimap()})
    connect_v2(@editor_map.sig_on_key("m"),  proc{ |x, y| toggle_minimap()})
    connect_v2(@editor_map.sig_on_key("g"),  proc{ |x, y| toggle_grid()})
    connect_v2(@editor_map.sig_on_key("4"),  proc{ |x, y| toggle_display_props()})
    connect_v2(@editor_map.sig_on_key("3"),  proc{ |x, y| show_foreground()})
    connect_v2(@editor_map.sig_on_key("2"),  proc{ |x, y| show_interactive()})
    connect_v2(@editor_map.sig_on_key("1"),  proc{ |x, y| show_background()})
  end

  def level_load()
    print "level_load\n"
  end

  def level_save()
    print "level_save\n"
  end

  def level_save_as()
    print "level_save_as\n"
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
    tilemap = @workspace.get_map().get_metadata().foreground;
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
