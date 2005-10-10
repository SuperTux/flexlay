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

require "layout_component.rb"
require "gui_specs.rb"

## GUI class which holds all the GUI components and the state of them
class GUI
  attr_reader :workspace, :gui, :tileselector, :objectselector, :tilegroup_menu, :layer_menu, :recent_files_menu

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

    post_initalize()
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

    @tileselector = @components.get('tileselector').component
    @tileselector.set_tileset($tileset)
    @tileselector.set_scale(0.75)
    @tileselector.set_tiles($tileset.get_tiles)
    @tileselector.show(true)
    
    @objectselector = @components.get('objectselector').component
    @objectselector.show(false)

    connect_v2_ObjectBrush_Point(@objectselector.sig_drop(), proc{|brush, pos| on_object_drop(brush, pos) })

    @load_dialog = SimpleFileDialog.new("Load Sector", "Load", "Cancel", @gui.get_component())
    @load_dialog.set_filename($datadir + "levels/")
    @save_dialog = SimpleFileDialog.new("Save Sector as...", "Save", "Cancel", @gui.get_component())
    @save_dialog.set_filename($datadir + "levels/")
    
    register_keybindings($keybinding_spec)

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

end

# EOF #
