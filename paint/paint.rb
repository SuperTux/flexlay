#!/usr/bin/ruby
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

require "flexlay_wrap"
include Flexlay_wrap

require "flexlay.rb"
require "sexpr.rb"

flexlay = Flexlay.new()

$screen_rect = CL_Rect.new(CL_Point.new(0, 0), CL_Size.new(1152, 864))

flexlay.init($screen_rect.get_width(), $screen_rect.get_height(), false)

$sketch_stroke_tool  = SketchStrokeTool.new()
$zoom_tool           = ZoomTool.new()

class PaintGUI
  attr_reader :workspace

  def initialize()
    @editor = Editor.new()
    @gui    = @editor.get_gui_manager()
    
    @editor_map = EditorMapComponent.new($screen_rect, @gui.get_component())
    @workspace  = Workspace.new($screen_rect.get_width(), $screen_rect.get_height())
    @editor_map.set_workspace(@workspace)
    @workspace.set_tool($sketch_stroke_tool.to_tool());

    @selector_window_main = Window.new(CL_Rect.new(CL_Point.new($screen_rect.get_width()-160, 5), 
                                                   CL_Size.new(128 + 6 + 10, 558)),
                                       "ColorPicker",
                                       @gui.get_component())
    @selector_window = @selector_window_main.get_client_area()

    @colorpicker = ColorPicker.new(CL_Rect.new(CL_Point.new(3, 3), CL_Size.new(128, 128)),
                                   @selector_window)

    connect_v1_Color(@colorpicker.sig_color_change(), proc{|color|
                       $sketch_stroke_tool.set_color(color)
                     })

    @bgcolorpicker = ColorPicker.new(CL_Rect.new(CL_Point.new(3, 250), CL_Size.new(128, 128)),
                                     @selector_window)

    connect_v1_Color(@bgcolorpicker.sig_color_change(), proc{|color|
                       @workspace.get_map().set_background_color(color)
                     })

    @size_slider = Slider.new(CL_Rect.new(CL_Point.new(3, 150), CL_Size.new(128, 16)), @selector_window)
    @size_slider.set_range(0.01, 2.0) # FIXME: using 0 size brush makes clanlib crashi
    @size_slider.set_value(1.0)
    connect_v1_float(@size_slider.sig_on_change, proc{|value|
                       # puts "Value: #{value}"
                       $sketch_stroke_tool.set_size(value)
                     })

#    @zoom_slider = Slider.new(CL_Rect.new(CL_Point.new(3, 182), CL_Size.new(128, 16)), @selector_window)
#    @zoom_slider.set_range(0.25, 10.0) # FIXME: using 0 size brush makes clanlib crashi
#    @zoom_slider.set_value(1.0)
#    connect_v1_float(@zoom_slider.sig_on_change, proc{|value|
#                       # puts "Value: #{value}"
#                       self.gui_set_zoom(value)
#                     })

    connect_v2(@editor_map.sig_on_key("escape"),  proc{ |x, y| puts "bye, bye"})
    connect_v2(@editor_map.sig_on_key("esc"),  proc{ |x, y| puts "bye, bye2"})
    connect_v2(@editor_map.sig_on_key("q"),  proc{ |x, y| $gui.quit()})
    connect_v2(@editor_map.sig_on_key("s"),  proc{ |x, y| $image.save("/tmp/test.scm")})
    connect_v2(@editor_map.sig_on_key("l"),  proc{ |x, y| 
                 $image = Image.new("/tmp/test.scm")
                 $image.activate($gui.workspace())
               })

#    $image.layers_count.times {|i|
#      button = CL_Button.new(CL_Rect.new(CL_Point.new(25*i+6, 500), CL_Size.new(25, 25)), "#{i}",
#                             @selector_window)
#      connect(button.sig_clicked(), proc{ $image.set_active_layer(i) })
#    }
  end

  def quit()
    @gui.quit()
  end

  def run()
    @gui.run()
  end
end

class Image
  attr_reader :layers
  
  def initialize(filename = nil)
    @editormap = EditorMap.new()
    @editormap.set_background_color(CL_Color.new(255, 255, 255))
    @layers  = []
    
    if filename then
      load(filename)
      set_active_layer(0)
    else
      add_layer()
      set_active_layer(0)
    end
  end
  
  def set_active_layer(i)
    if (i >= 0 && i < layers.size) then
      SketchLayer.set_current(layers[i])
    end
  end

  def add_layer()
    layer = SketchLayer.new()
    @layers.push(layer)
    @editormap.add_layer(layer.to_layer()) 
    puts "Add layer: #{layer}" 
    return layer
  end

  def layers_count()
    return layers.size
  end

  def activate(workspace)
    workspace.set_map(@editormap)
    connect(@editormap.sig_change(), proc{$gui.on_map_change()})
  end

  def load(filename)
    tree = sexpr_read_from_file(filename)
    if tree == nil
        raise("Couldn't load level: %s" % filename)
    end
    
    # Skip flexlay-paint tag
    tree = tree[1..-1]

    while (not tree.empty?) do
      (tag, *data) = tree[0]

      puts "Tag: #{tag}"
      if tag == "layer" then
        parse_layer(data)
      end

      tree = tree[1..-1]
    end
  end

  def parse_layer(tree)
    layer = add_layer()

    while not tree.empty? do
      (tag, *data) = tree[0]
      
      if tag == "stroke" then
        parse_stroke(layer, data)
      end

      tree = tree[1..-1]
    end
    puts ""
  end

  def parse_stroke(layer, tree)
    stroke = Stroke.new()

    sprite_drawer = SpriteStrokeDrawer.new()
    sprite_drawer.set_sprite(make_sprite("../data/images/brush/brush.png"))
    sprite_drawer.set_color(CL_Color.new(0, 0, 0, 155))
    sprite_drawer.set_size(1.0)
    stroke.set_drawer(sprite_drawer.to_drawer())

    while not tree.empty? do
      (tag, *data) = tree[0]     

      if tag == "dab" then
        time     = get_value_from_tree(["time", "_"], data, 0)
        position = get_value_from_tree(["position"],  data, [0, 0])
        pressure = get_value_from_tree(["pressure", "_"],  data, 1.0)
        tilt     = get_value_from_tree(["tilt", "_"],  data, [0, 0])

        # FIXME: No tilt support
        stroke.add_dab(Dab.new(position[0], position[1], pressure))
      elsif tag == "drawer" then
        if data[0][0] == "sprite-stroke-drawer" then
          data = data[0][1..-1]
          mode    = get_value_from_tree(["mode", "_"], data, SpriteStrokeDrawer::DM_NORMAL)
          spacing = get_value_from_tree(["spacing", "_"], data, 15.0)
          size    = get_value_from_tree(["size", "_"],    data,  1.0)
          color   = get_value_from_tree(["color"],    data, [0, 255, 0, 255])
          brush   = get_value_from_tree(["brush", "_"],    data, "brush.png")

          drawer = SpriteStrokeDrawer.new()
          drawer.set_spacing(spacing)
          drawer.set_mode(mode)
          drawer.set_size(size)
          drawer.set_color(CL_Color.new(color[0], color[1], color[2], color[3]))
          drawer.set_sprite(make_sprite("../data/images/brush/#{brush}"))
          stroke.set_drawer(drawer.to_drawer)
        else
          puts "Error: Unknown drawer: #{data[0][0]}" 
        end
      end

      tree = tree[1..-1]
    end
    
    print "."
    $stdout.flush
    layer.add_stroke(stroke)
  end

  def save(filename)
    f = File.new(filename, "w")
    f.puts "(flexlay-paint"
    @layers.each { |layer|
      f.puts "(layer"
      layer.get_strokes().each{|stroke|
        f.puts "  (stroke"
        
        # FIXME: This won't work with a real smartptr!
        sprite_stroke_drawer = SpriteStrokeDrawer.new(stroke.get_drawer())

        f.puts "      (drawer (sprite-stroke-drawer"
        f.puts "                 (spacing #{sprite_stroke_drawer.get_spacing})"
        f.puts "                 (size    #{sprite_stroke_drawer.get_size})"
        f.puts "                 (color   "\
        "#{sprite_stroke_drawer.get_color.get_red} " \
        "#{sprite_stroke_drawer.get_color.get_green} " \
        "#{sprite_stroke_drawer.get_color.get_blue} " \
        "#{sprite_stroke_drawer.get_color.get_alpha})"
        f.puts "                 (brush   \"brush.png\")"
        f.puts "       ))"

        stroke.get_dabs().each{|dab|
          f.puts "    (dab"
          f.puts "      (time     #{dab.time})"
          f.puts "      (position #{dab.pos.x} #{dab.pos.y})"
          f.puts "      (pressure #{dab.pressure})"
          f.puts "      (tilt     #{dab.tilt.x} #{dab.tilt.y})"
          f.puts ")"
        }
        f.puts ")"
      }
      f.puts ")"
      f.puts ")"
      f.close()
    }
  end
end

$gui   = PaintGUI.new()

$image = Image.new()
# $image = Image.new("example2.scm")
# $image = Image.new()
# $image.add_layer()
# $image.add_layer()

$image.activate($gui.workspace)

$gui.run()

# FIXME: Can't deinit flexlay, since we would crash then, refcouting throuble...
# flexlay.deinit()
# puts "And now we crash"

# EOF #
