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

$screen_rect = CL_Rect.new(CL_Point.new(0, 0), CL_Size.new(800, 600))
# $screen_rect = CL_Rect.new(CL_Point.new(0, 0), CL_Size.new(1152, 864))

flexlay.init($screen_rect.get_width(), $screen_rect.get_height(), false)

$sketch_stroke_tool  = SketchStrokeTool.new()
$layer_move_tool     = LayerMoveTool.new()
$zoom_tool           = ZoomTool.new()

class PaintGUI
  attr_reader :workspace, :selector_window

  def initialize()
    @editor = Editor.new()
    @gui    = @editor.get_gui_manager()
    
    @editor_map = EditorMapComponent.new($screen_rect, @gui.get_component())
    @workspace  = Workspace.new($screen_rect.get_width(), $screen_rect.get_height())
    @editor_map.set_workspace(@workspace)
    # @workspace.set_tool($sketch_stroke_tool.to_tool());
    @workspace.set_tool($layer_move_tool.to_tool());

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

    @bgcolorpicker = ColorPicker.new(CL_Rect.new(CL_Point.new(3, 300), CL_Size.new(128, 128)),
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

    @brush_hardness = Slider.new(CL_Rect.new(CL_Point.new(3, 170), CL_Size.new(128, 16)),
                                 @selector_window)
    @brush_hardness.set_range(0.0, 1.0)
    @brush_hardness.set_value(0.75)
    connect_v1_float(@brush_hardness.sig_on_change, proc{|value|
                       drawer = SpriteStrokeDrawer.new($sketch_stroke_tool.get_drawer())
                       GeneratedBrush.new(drawer.get_brush()).set_hardness(value)
                     })

    @brush_spikes = Slider.new(CL_Rect.new(CL_Point.new(3, 190), CL_Size.new(128, 16)),
                                 @selector_window)
    @brush_spikes.set_range(2, 20)
    @brush_spikes.set_value(2)
    connect_v1_float(@brush_spikes.sig_on_change, proc{|value|
                       drawer = SpriteStrokeDrawer.new($sketch_stroke_tool.get_drawer())
                       GeneratedBrush.new(drawer.get_brush()).set_spikes(value.to_i)
                     })

    @brush_aspects = Slider.new(CL_Rect.new(CL_Point.new(3, 210), CL_Size.new(128, 16)),
                                 @selector_window)
    @brush_aspects.set_range(0.1, 10)
    @brush_aspects.set_value(1)
    connect_v1_float(@brush_aspects.sig_on_change, proc{|value|
                       drawer = SpriteStrokeDrawer.new($sketch_stroke_tool.get_drawer())
                       GeneratedBrush.new(drawer.get_brush()).set_aspect_ratio(value)
                     })

    @brush_angles = Slider.new(CL_Rect.new(CL_Point.new(3, 230), CL_Size.new(128, 16)),
                                 @selector_window)
    @brush_angles.set_range(0, 360)
    @brush_angles.set_value(0)
    connect_v1_float(@brush_angles.sig_on_change, proc{|value|
                       drawer = SpriteStrokeDrawer.new($sketch_stroke_tool.get_drawer())
                       GeneratedBrush.new(drawer.get_brush()).set_angle(value)
                     })

    @brush_shape_circle  = CL_Button.new(CL_Rect.new(CL_Point.new(5, 250), CL_Size.new(40, 25)), "Circ", @selector_window)
    @brush_shape_rect    = CL_Button.new(CL_Rect.new(CL_Point.new(45, 250), CL_Size.new(40, 25)), "Squa", @selector_window)
    @brush_shape_diamond = CL_Button.new(CL_Rect.new(CL_Point.new(85, 250), CL_Size.new(40, 25)), "Diam", @selector_window)

    connect(@brush_shape_circle.sig_clicked(), proc{ 
              drawer = SpriteStrokeDrawer.new($sketch_stroke_tool.get_drawer())
              GeneratedBrush.new(drawer.get_brush()).set_shape(BRUSH_SHAPE_CIRCLE)
            })
    connect(@brush_shape_rect.sig_clicked(), proc{ 
              drawer = SpriteStrokeDrawer.new($sketch_stroke_tool.get_drawer())
              GeneratedBrush.new(drawer.get_brush()).set_shape(BRUSH_SHAPE_SQUARE)
            })
    connect(@brush_shape_diamond.sig_clicked(), proc{ 
              drawer = SpriteStrokeDrawer.new($sketch_stroke_tool.get_drawer())
              GeneratedBrush.new(drawer.get_brush()).set_shape(BRUSH_SHAPE_DIAMOND)
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

    connect_v2(@editor_map.sig_on_key("g"),  proc{ |x, y|
                 $gui.workspace.get_gc_state.set_rotation($gui.workspace.get_gc_state.get_rotation() - 10)
               })
    connect_v2(@editor_map.sig_on_key("c"),  proc{ |x, y| 
                 $gui.workspace.get_gc_state.set_rotation($gui.workspace.get_gc_state.get_rotation() + 10)
               })

    @normal_mode = CL_Button.new(CL_Rect.new(CL_Point.new(5, 500), CL_Size.new(40, 25)), "Norm", @selector_window)
    @erase_mode  = CL_Button.new(CL_Rect.new(CL_Point.new(45, 500), CL_Size.new(40, 25)), "Erase", @selector_window)
    @add_mode    = CL_Button.new(CL_Rect.new(CL_Point.new(85, 500), CL_Size.new(40, 25)), "Add", @selector_window)
    @shader_mode = CL_Button.new(CL_Rect.new(CL_Point.new(125, 500), CL_Size.new(40, 25)), "Shad", @selector_window)

    connect(@normal_mode.sig_clicked(), proc{ 
              drawer = SpriteStrokeDrawer.new($sketch_stroke_tool.get_drawer())
              drawer.set_mode(SpriteStrokeDrawer::DM_NORMAL)
            })
    connect(@erase_mode.sig_clicked(),  proc{ 
              drawer = SpriteStrokeDrawer.new($sketch_stroke_tool.get_drawer())
              drawer.set_mode(SpriteStrokeDrawer::DM_ERASE)
            })
    connect(@add_mode.sig_clicked(),    proc{
              drawer = SpriteStrokeDrawer.new($sketch_stroke_tool.get_drawer())
              drawer.set_mode(SpriteStrokeDrawer::DM_ADDITION)
            })
    connect(@shader_mode.sig_clicked(),    proc{
              drawer = SpriteStrokeDrawer.new($sketch_stroke_tool.get_drawer())
              drawer.set_mode(SpriteStrokeDrawer::DM_SHADER)
            })

    button_panel = ButtonPanel.new(0, 0, 33, 33*3, false, @gui.get_component)
    button_panel.add_icon("../data/images/tools/stock-tool-pencil-22.png", proc{ 
                            @workspace.set_tool($sketch_stroke_tool.to_tool())
                          })
    button_panel.add_icon("../data/images/tools/stock-tool-move-22.png", proc{ 
                            @workspace.set_tool($layer_move_tool.to_tool())
                          })
    button_panel.add_icon("../data/images/tools/stock-tool-zoom-22.png", proc{ 
                            @workspace.set_tool($zoom_tool.to_tool())
                          })
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
      add_layer()
      add_layer()
      set_active_layer(0)
    end
  end
  
  def set_active_layer(i)
    if (i >= 0 && i < layers.size) then
      BitmapLayer.set_current(layers[i])
    end
  end

  def add_layer()
    layer = BitmapLayer.new(640, 480)
    layer.to_layer().set_pos(CL_Pointf.new(rand(100)-50, rand(100)-50))
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
    puts "REst: #{tree.length}"
    while not tree.empty? do
      (tag, *data) = tree[0]

      puts "Tag: #{tag}"

      if tag == "layer" then
        puts "MUMU LAYER" 
        parse_layer(data)
      end

      tree = tree[1..-1]
      puts "REst2: #{tree.length}"
    end

    set_active_layer(0)
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
    # FIXME: insert loader for brush here
    sprite_drawer.set_brush(GeneratedBrush.new(BRUSH_SHAPE_CIRCLE, 
                                               32,  # radius
                                               2,   # spikes
                                               0.75, # hardness
                                               1.0, # aspect
                                               0).to_brush()) # angle
    
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
        stroke.add_dab(Dab.new(position[0].to_f, position[1].to_f, pressure.to_f))
      elsif tag == "drawer" then
        if data[0][0] == "sprite-stroke-drawer" then
          data = data[0][1..-1]
          mode    = get_value_from_tree(["mode", "_"], data, SpriteStrokeDrawer::DM_NORMAL)
          spacing = get_value_from_tree(["spacing", "_"], data, 15.0)
          size    = get_value_from_tree(["size", "_"],    data,  1.0)
          color   = get_value_from_tree(["color"],    data, [0, 255, 0, 255])
          brush   = get_value_from_tree(["brush", "generated-brush"],    data, [])

          drawer = SpriteStrokeDrawer.new()
          drawer.set_spacing(spacing)
          drawer.set_mode(mode)
          drawer.set_size(size)
          drawer.set_color(CL_Color.new(color[0], color[1], color[2], color[3]))
          drawer.set_brush(GeneratedBrush.new(get_value_from_tree(["shape", "_"], brush, 0),
                                              get_value_from_tree(["radius", "_"], brush, 32),
                                              get_value_from_tree(["spikes", "_"], brush, 2),
                                              get_value_from_tree(["hardness", "_"], brush, 0.75),
                                              get_value_from_tree(["aspect-ratio", "_"], brush, 1.0),
                                              get_value_from_tree(["angle", "_"], brush, 0)).to_brush)
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
        f.puts "                 (mode    #{sprite_stroke_drawer.get_mode})"
        f.puts "                 (spacing #{sprite_stroke_drawer.get_spacing})"
        f.puts "                 (size    #{sprite_stroke_drawer.get_size})"
        f.puts "                 (color   "\
        "#{sprite_stroke_drawer.get_color.get_red} " \
        "#{sprite_stroke_drawer.get_color.get_green} " \
        "#{sprite_stroke_drawer.get_color.get_blue} " \
        "#{sprite_stroke_drawer.get_color.get_alpha})"
        # FIXME: This won't work with multilpe brush types
        brush = GeneratedBrush.new(sprite_stroke_drawer.get_brush())
        f.puts "                 (brush   (generated-brush"
        f.puts "                            (shape  #{brush.get_shape})"
        f.puts "                            (radius #{brush.get_radius})"
        f.puts "                            (spikes #{brush.get_spikes})"
        f.puts "                            (hardness #{brush.get_hardness})"
        f.puts "                            (aspect-ratio #{brush.get_aspect_ratio})"
        f.puts "                            (angle #{brush.get_angle})"
        f.puts "                  ))"
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
    }
    f.puts ")"
    f.close()
  end
end

$gui   = PaintGUI.new()

$image = Image.new()
# $image = Image.new("example2.scm")
# $image = Image.new()
# $image.add_layer()
# $image.add_layer()

$image.activate($gui.workspace)

drawer = SpriteStrokeDrawer.new($sketch_stroke_tool.get_drawer())
drawer.set_brush(GeneratedBrush.new(BRUSH_SHAPE_CIRCLE, 
                                    32,  # radius
                                    2,   # spikes
                                    0.75, # hardness
                                    1.0, # aspect
                                    0).to_brush()) # angle

$image.layers_count.times {|i|
  button = CL_Button.new(CL_Rect.new(CL_Point.new(25*i+6, 450), CL_Size.new(25, 25)), "#{i}",
                         $gui.selector_window)
  connect(button.sig_clicked(), proc{ $image.set_active_layer(i) })
}

$gui.run()

# FIXME: Can't deinit flexlay, since we would crash then, refcouting throuble...
# flexlay.deinit()
# puts "And now we crash"

# EOF #
