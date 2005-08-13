##  $Id$
##   ______ __               ___
##  |   ___|  |.-----.--.--.|   | .---.-.--.--.
##  |   ___|  ||  -__|_   _||   |_|  _  |  |  |
##  |__|   |__||_____|__.__||_____|___._|___  |
##                                      |_____|
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
##  Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
##  02111-1307, USA.

require "gameobjects.rb"
require "gui_specs.rb"

## GUI class which holds all the GUI components and the state of them
class GUI
  attr_reader :workspace, :gui, :tileselector, :objectselector, :tilegroup_menu, :layer_menu

  def run()
    ## Enter main loop here
    @gui.run()
  end

  def initialize()
    @gui = GUIManager.new()

    myrect      = CL_Rect.new(CL_Point.new(0, 56), CL_Size.new($screen_width - 800 +  665,
                                                               $screen_height - 600 + 488+56))
    @editor_map = EditorMapComponent.new(myrect, @gui.get_component())
    @workspace  = Workspace.new()
    @editor_map.set_workspace(@workspace)

    @button_panel = ButtonPanel.new_from_spec(0, 23, $screen_width, 33, true, $buttonpanel_spec, @gui.get_component)

    @button_panel.items["undo"].disable()
    @button_panel.items["redo"].disable()

    @layer_menu = Menu.new(CL_Point.new(32*15+2, 54), @gui.get_component())
    @layer_menu.add_item($mysprite, "Show all",          proc{ show_all() })
    @layer_menu.add_item($mysprite, "Show current",      proc{ show_current() })
    @layer_menu.add_item($mysprite, "Show only current", proc{ show_only_current() })
    
    @toolbar = ButtonPanel.new_from_spec(0, 23+33, 33, 32*4+2, false, $toolbar_spec, @gui.get_component)
    @menu    = CL_Menu.new_from_spec($menu_spec, @gui.get_component)

    # FIXME: Having position in the Menus here is EXTREMLY ugly
    @tilegroup_menu = Menu.new(CL_Point.new(35*15+2, 54), @gui.get_component())
    @tilegroup_menu.add_item($mysprite, "All Tiles", proc{@tileselector.set_tiles($tileset.get_tiles())})
    $tileset.tilegroups.each { |tilegroup|
      @tilegroup_menu.add_item($mysprite, tilegroup.name, proc{@tileselector.set_tiles(tilegroup.tiles)})
    }

    @selector_window = Panel.new(CL_Rect.new(CL_Point.new($screen_width-128-64-6, 23+33), 
                                             CL_Size.new(128 + 64 + 6, $screen_height - 600 + 558)),
                                 @gui.get_component())

    @minimap = Minimap.new(@editor_map, CL_Rect.new(CL_Point.new(3,  $screen_height - 600 + 552 - 144 - 12), 
                                                    CL_Size.new(192, 144)),
                           @selector_window)

    @tileselector = TileSelector.new(CL_Rect.new(CL_Point.new(3, 3), 
                                                 CL_Size.new(128+64, $screen_height - 600 + 552 - 144 - 3)),
                                     @selector_window)
    @tileselector.set_tileset($tileset)
    @tileselector.set_scale(0.75)
    @tileselector.set_tiles($tileset.tilegroups[0].tiles)
    # @tileselector.set_tiles($tileset.get_tiles())
    @tileselector.show(true)
    
    @objectselector = ObjectSelector.new(CL_Rect.new(0, 0, 128, $screen_height - 600 + 552 - 144 - 3), 42, 42, @selector_window)
    @objectselector.show(false)

    connect_v2_ObjectBrush_Point(@objectselector.sig_drop(), proc{|brush, pos| on_object_drop(brush, pos) })

    $gameobjects.each do |object|
      @objectselector.add_brush(ObjectBrush.new(make_sprite($datadir + object[1]),
                                                make_metadata(object)))
    end

    @load_dialog = SimpleFileDialog.new("Load Sector", "Load", "Cancel", @gui.get_component())
    @load_dialog.set_filename($datadir + "levels/")
    @save_dialog = SimpleFileDialog.new("Save Sector as...", "Save", "Cancel", @gui.get_component())
    @save_dialog.set_filename($datadir + "levels/")
    
    register_keybindings($keybinding_spec)

    set_zoom(0.5)
  end

  def register_keybindings(spec)
    spec.each{ |(key, callback)|
      connect_v2(@editor_map.sig_on_key(key), callback)
    }
  end

  def on_map_change()
    if (@workspace.get_map().undo_stack_size() > 0)
      @button_panel.items["undo"].enable()
    else
      @button_panel.items["undo"].disable()
    end

    if (@workspace.get_map().redo_stack_size() > 0)
      @button_panel.items["redo"].enable()
    else
      @button_panel.items["redo"].disable()
    end
  end

  def show_layer(layer)
    if layer == :background then
      @button_panel.items["background"].set_down();
      TilemapLayer.set_current(@workspace.get_map().get_metadata().background)
    else
      @button_panel.items["background"].set_up();
    end

    if layer == :interactive then
      @button_panel.items["interactive"].set_down();
      TilemapLayer.set_current(@workspace.get_map().get_metadata().interactive)
    else
      @button_panel.items["interactive"].set_up();
    end

    if layer == :foreground then
      @button_panel.items["foreground"].set_down();
      TilemapLayer.set_current(@workspace.get_map().get_metadata().interactive)
    else
      @button_panel.items["foreground"].set_up();
    end
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

  def toggle_grid()
    tilemap = @workspace.get_map().get_data().layers[0];
    tilemap.set_draw_grid(!tilemap.get_draw_grid())

    if tilemap.get_draw_grid()
      @button_panel.items["grid"].set_down()
    else
      @button_panel.items["grid"].set_up()
    end
  end

  def set_tool_icon(tool)
    if tool == :tilemap_paint  then  @toolbar.items["paint"].set_down()  else @toolbar.items["paint"].set_up()  end
    if tool == :tilemap_select then  @toolbar.items["select"].set_down() else @toolbar.items["select"].set_up() end
    if tool == :zoom           then  @toolbar.items["zoom"].set_down()   else @toolbar.items["zoom"].set_up()   end
    if tool == :object_select  then  @toolbar.items["object"].set_down() else @toolbar.items["object"].set_up() end

    if tool == :tilemap_paint then
      @objectselector.show(false)
      @tileselector.show(true)
    elsif tool == :object_select then
      @objectselector.show(true)
      @tileselector.show(false)
    end
  end
  
  def set_zoom(zoom)
    gc = @editor_map.get_gc_state()
    pos = gc.get_pos()
    gc.set_zoom(zoom)
    gc.set_pos(pos)
  end

  def new_level()
    level = @workspace.get_map().get_metadata()
    dialog = GenericDialog.new("Create New Sector", @gui.get_component())
    dialog.add_int("Width: ", level.width)
    dialog.add_int("Height: ", level.height)
    dialog.add_int("X: ", 0)
    dialog.add_int("Y: ", 0)
    dialog.set_callback(proc{|w, h, x, y| 
                          level.resize(CL_Size.new(w, h), CL_Point.new(x, y))})
  end

  def on_object_drop(brush, pos)
    pos  = @editor_map.screen2world(pos)
    data = get_ruby_object(brush.get_data())
    create_gameobject($gui.workspace.get_map().get_metadata().objects, data, pos)
  end

  def create_gameobject(objmap, data, pos, sexpr = [])
    case data[2] 
    when "sprite" 
      obj = ObjMapSpriteObject.new(make_sprite($datadir + data[1]), pos, make_metadata(nil))
      gobj = data[3].call(obj, sexpr) 
      obj.to_object.set_metadata(make_metadata(gobj))
    end
    
    cmd = ObjectAddCommand.new(objmap)
    cmd.add_object(obj.to_object);
    $gui.workspace.get_map().execute(cmd.to_command());
    return obj
  end
end

# EOF #
