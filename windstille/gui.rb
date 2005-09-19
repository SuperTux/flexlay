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
require "layout_component.rb"
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
    
    @components = LayoutComponent.create_from_sexpr(CL_Rect.new(0,0, $screen_width, $screen_height),
                                                    SExpression.new($guilayout_spec),
                                                   @gui.get_component())
    
    connect_v2_graceful($flexlay.sig_resize(), proc{|w, h|
                          @components.set_size(w, h)
                        })
  end

  def post_initalize()
    @editor_map = @components.get('editormap').component
    @workspace  = Workspace.new()
    @editor_map.set_workspace(@workspace)

    @button_panel = @components.get('buttonpanel').component

    @button_panel.items["undo"].disable()
    @button_panel.items["redo"].disable()

    @layer_menu = Menu.new(CL_Point.new(32*15+2, 54), @gui.get_component())
    @layer_menu.add_item("Show all",          proc{ show_all() })
    @layer_menu.add_item("Show current",      proc{ show_current() })
    @layer_menu.add_item("Show only current", proc{ show_only_current() })
    
    @toolbar = ButtonPanel.new_from_spec(0, 23+33, 33, 32*4+2, false, $toolbar_spec, @gui.get_component)
    @menu    = @components.get('menubar').component

    # FIXME: Having position in the Menus here is EXTREMLY ugly
    @tilegroup_menu = Menu.new(CL_Point.new(35*15+2, 54), @gui.get_component())
    @tilegroup_menu.add_item("All Tiles", proc{@tileselector.set_tiles($tileset.get_tiles())})
    $tileset.tilegroups.each { |tilegroup|
      @tilegroup_menu.add_item(tilegroup.name, proc{@tileselector.set_tiles(tilegroup.tiles)})
    }

    @tileselector = @components.get('tileselector').component
    @tileselector.set_tileset($tileset)
    @tileselector.set_scale(0.75)
    @tileselector.set_tiles($tileset.tilegroups[0].tiles)
    # @tileselector.set_tiles($tileset.get_tiles())
    @tileselector.show(true)
    
    @objectselector = @components.get('objectselector').component
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
    puts "Mapchange" 
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

    if layer == :interactivebackground then
      @button_panel.items["interactivebackground"].set_down();
      TilemapLayer.set_current(@workspace.get_map().get_metadata().interactivebackground)
    else
      @button_panel.items["interactivebackground"].set_up();
    end

    if layer == :foreground then
      @button_panel.items["foreground"].set_down();
      TilemapLayer.set_current(@workspace.get_map().get_metadata().foreground)
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
    dialog.add_int("Width: ",  60)
    dialog.add_int("Height: ", 40)
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

  def layer_properties()
    tilemap = TilemapLayer.current()

    dialog = GenericDialog.new("Edit Tilemap Properties", @gui.get_component())
    dialog.add_string("Name: ", tilemap.get_metadata().name)
    dialog.add_int("XOffset: ", tilemap.get_metadata().x_offset)
    dialog.add_int("YOffset: ", tilemap.get_metadata().y_offset)
    dialog.add_int("Z-Pos: ",   tilemap.get_metadata().z_pos)

    dialog.set_callback(proc{|name, x_offset, y_offset, z_pos| 
                          tilemap.get_metadata().name      = name
                          tilemap.get_metadata().x_offset = x_offset 
                          tilemap.get_metadata().y_offset = y_offset 
                          tilemap.get_metadata().z_pos    = z_pos 
                        })
  end
end

# EOF #
