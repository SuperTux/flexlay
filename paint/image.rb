class Image
  attr_reader :layers
  attr_accessor :editormap

  def initialize(filename = nil)
    @editormap = EditorMap.new()
    @editormap.set_bounding_rect(CL_Rect.new(0, 0, 1024, 768))
    @editormap.set_background_color(CL_Color.new(255, 255, 255))

    @objectmap = ObjectLayer.new()
    @editormap.add_layer(@objectmap.to_layer()) 

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
    if (i >= 0 && i < @layers.size) then
      BitmapLayer.set_current(@layers[i])
    end
  end

  def add_layer(filename = nil)
    if filename then
      layer = BitmapLayer.new(make_pixelbuffer(filename))
      @layers.push(layer)
      @objectmap.add_object(layer.to_object())
      return layer
    else
      layer = BitmapLayer.new(1024, 768)
      @layers.push(layer)
      @objectmap.add_object(layer.to_object())
      return layer
    end
  end

  def layers_count()
    return @layers.size
  end

  def activate(workspace)
    workspace.set_map(@editormap)
    BitmapLayer.set_current(@layers[0])
    ObjectLayer.set_current(@objectmap)
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
    DrawerProperties.current().set_brush(GeneratedBrush.new(BRUSH_SHAPE_CIRCLE, 
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

# EOF #
