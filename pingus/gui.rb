##  $Id$
##
##  Flexlay - A Generic 2D Game Editor
##  Copyright (C) 2005 Ingo Ruhnke <grumbel@gmx.de>
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

class GUI
  attr_reader :workspace, :objectselector;

  def initialize()
    @editor = Editor.new()
    @gui    = @editor.get_gui_manager()

    myrect      = CL_Rect.new(CL_Point.new(0, 56), CL_Size.new(665, 488+56))
    @editor_map = EditorMapComponent.new(myrect, @gui.get_component())
    @workspace  = Workspace.new(myrect.get_width(), myrect.get_height())
    @editor_map.set_workspace(@workspace)

    @selector_window = Panel.new(CL_Rect.new(CL_Point.new(800-134, 23+33), CL_Size.new(128 + 6, 558)),
                                 @gui.get_component())

    @objectselector = ObjectSelector.new(CL_Rect.new(0, 0, 128, 600), 42, 42, @selector_window)
    @objectselector.show(true)
    connect_v2_ObjectBrush_Point(@objectselector.sig_drop(), method(:on_object_drop))

    create_menu()
    create_toolbar()
    create_button_panel()

    connect_v2(@editor_map.sig_on_key("e"), 
               proc{ |x, y|
                 puts "Content: #{x}, #{y}"
                 $objmap_select_tool.get_selection().each {|i|
                   i.get_data().property_dialog()
                 }})
  end

  def run()
    @gui.run()
  end

  def create_button_panel()
    @button_panel = ButtonPanel.new(0, 23, 800, 33, true, @gui.get_component)
    @button_panel.add_icon("../data/images/icons24/stock_new.png")
    @button_panel.add_icon("../data/images/icons24/stock_open.png", proc{ level_load() })
    @button_panel.add_small_icon("../data/images/icons24/downarrow.png", proc{ $controller.recent_files_menu.run() })
    @button_panel.add_icon("../data/images/icons24/stock_save.png", proc{ level_save() })
    @button_panel.add_icon("../data/images/icons24/stock_save_as.png", proc{ level_save_as() })
    @button_panel.add_seperator()
    @button_panel.add_icon("../data/images/icons24/stock_copy.png")
    @button_panel.add_icon("../data/images/icons24/stock_paste.png")
    @button_panel.add_seperator()
    @undo_icon = @button_panel.add_icon("../data/images/icons24/stock_undo.png", proc{ @workspace.get_map().undo() })
    @redo_icon = @button_panel.add_icon("../data/images/icons24/stock_redo.png", proc{ @workspace.get_map().redo() })
  end

  def create_toolbar()
    @toolbar = ButtonPanel.new(0, 23+33, 33, 32*4+2, false, @gui.get_component)
    @object = @toolbar.add_icon("../data/images/tools/stock-tool-clone-22.png", 
                                proc{ workspace.set_tool($objmap_select_tool.to_tool())})

    @toolbar = ButtonPanel.new(0, 23+33*2, 33, 32*4+2, false, @gui.get_component)
    @object = @toolbar.add_icon("../data/images/tools/stock-tool-zoom-22.png", 
                                proc{ workspace.set_tool($zoom_tool.to_tool())})
  end

  def create_menu()
    @menu = CL_Menu.new(@gui.get_component())
    @menu.add_item("File/Quit",  proc{ @gui.quit })
    
    @menu.add_item("Zoom/1:4 (25%) ",  proc{ self.gui_set_zoom(0.25) })
    @menu.add_item("Zoom/1:2 (50%) ",  proc{ self.gui_set_zoom(0.5) })
    @menu.add_item("Zoom/1:1 (100%) ", proc{ self.gui_set_zoom(1.0) }) 
    @menu.add_item("Zoom/2:1 (200%) ", proc{ self.gui_set_zoom(2.0) })
    @menu.add_item("Zoom/4:1 (400%) ", proc{ self.gui_set_zoom(4.0) })
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

  def on_object_drop(brush, pos)
    pos  = @editor_map.screen2world(pos)
    (type, sprite) = get_ruby_object(brush.get_data()) # object type 'groundpiece'
    
    worldobj = WorldObj.new(type, sprite)

    puts "String: #{worldobj.get_image().inspect}"
    obj = ObjMapSpriteObject.new(CL_Sprite.new(worldobj.get_image(), $resources), 
                                 pos, 
                                 make_metadata(worldobj))
    worldobj.set_data(obj)

    workspace.get_map().get_metadata().objects.add_object(obj.to_object())
  end

  def get_component()
    return @gui.get_component()
  end

end

# EOF #
