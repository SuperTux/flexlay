class Sector
  parent    = nil
  name      = nil
  song      = nil
  gravity   = 10.0
  
  width  = nil
  height = nil
  
  background  = nil
  interactive = nil
  foreground  = nil
  
  objects   = nil
  sketch    = nil
  editormap = nil

  attr_reader   :sketch, :objects, :background, :interactive, :foreground, :parent, :width, :height
  attr_accessor :name, :song, :gravity
  
  def initialize(parent)
    @parent = parent
  end

  def get_level()
    return @parent
  end

  def resize(size, pos)
    @width  = size.width
    @height = size.height
    @background.resize(size, pos)
    @interactive.resize(size, pos)
    @foreground.resize(size, pos)
  end

  def new_from_size(name, width, height)
    @name = name
    @song = "<No Song>"
    @gravity = 10.0
    
    @width  = width
    @height = height
    
    @foreground  = TilemapLayer.new($tileset, @width, @height)
    @interactive = TilemapLayer.new($tileset, @width, @height)
    @background  = TilemapLayer.new($tileset, @width, @height)       
    @objects = ObjectLayer.new()
    @sketch  = SketchLayer.new()

    @editormap = EditorMap.new()
    @editormap.set_background_color(CL_Color.new(255, 255, 255))
    @editormap.add_layer(@background.to_layer())
    @editormap.add_layer(@interactive.to_layer())
    @editormap.add_layer(@objects.to_layer())
    @editormap.add_layer(@foreground.to_layer())
    @editormap.add_layer(@sketch.to_layer())
    # FIXME: Data might not get freed since its 'recursively' refcounted
    @editormap.set_metadata(self)
    return self
  end

  def load_v1(data)
    @name = "<No Name>"
    @song = "supertux-1.ogg"
    @gravity = 10.0
    
    @width  = get_value_from_tree(["width", "_"], data, 20)
    @height = get_value_from_tree(["height""_"], data, 15)
    
    @foreground  = TilemapLayer.new($tileset, @width, @height)
    @foreground.set_data(get_value_from_tree(["foreground-tm"], data, []))
    
    @interactive = TilemapLayer.new($tileset, @width, @height)
    @interactive.set_data(get_value_from_tree(["interactive-tm"], data, []))
    
    @background  = TilemapLayer.new($tileset, @width, @height)
    @background.set_data(get_value_from_tree(["background-tm"], data, []))
    
    @objects = ObjectLayer.new()
    for i in get_value_from_tree(["objects"], data, [])
      type = i[0]
      x = get_value_from_tree(["x", "_"], i[1..-1], 0)
      y = get_value_from_tree(["y", "_"], i[1..-1], 0)
      print("Object position: ", type, " ", x, " ", y, "\n")
      object = $game_objects.find{|x| x[0] == type}
      print "Resolved object: ", object, "\n"
      if object
        # fixme
        #        @objects.add_object(ObjMapSpriteObject.new(make_sprite($datadir + object[1]),
        #                                                   CL_Point.new(x, y),
        #                                                   make_metadata(BadGuy.new(object[0]))).to_object())
      else
        print "Error: Couldn't resolve object type: ", type, "\n"
      end
    end
    
# FIXME: doesn't work
#     for i in get_value_from_tree(["reset-points"], data, [])
#       type = i[0]
#       x = get_value_from_tree(["x", "_"], i[1..-1], [])
#       y = get_value_from_tree(["y", "_"], i[1..-1], [])
#       object = $game_objects.find("resetpoint")
#       @objects.add_object(ObjMapSpriteObject.new(make_sprite($datadir + object[1]),
#                                                  CL_Point.new(x, y),
#                                                  make_metadata(BadGuy.new(object[0]))).to_object())
#     end

    @editormap = EditorMap.new()
    @editormap.add_layer(@background.to_layer())
    @editormap.add_layer(@interactive.to_layer())
    @editormap.add_layer(@objects.to_layer())
    @editormap.add_layer(@foreground.to_layer())
    # FIXME: Data might not get freed since its 'recursively' refcounted
    @editormap.set_metadata(self)
  end
  
  def load_v2(data)
    @name = "<No Name>"
    @song = "supertux-1.ogg"
    @gravity = 10.0
    
    @width  = 20
    @height = 15

    @background  = nil
    @interactive = nil
    @foreground  = nil
    
    @objects = ObjectLayer.new()
    @sketch = SketchLayer.new()

    for i in data
      (name,data) = i[0], i[1..-1]
      if name == "name"
        @name = data[0]
      elsif name == "gravity"
        @gravity = data[0]
      elsif name == "playerspawn"
        print "playerspawn unhandled"
      elsif name == "strokelayer"
        data.each {|(el, *rest)|
          # puts "#{el} -> #{rest}"
          if (el == "stroke") then
            stroke = Stroke.new()
            rest.each {|(el2, *rest2)|
              if el2 == "point" then
                stroke.add_dab(Dab.new(rest2[0], rest2[1]))
              elsif el2 == "color"
                stroke.set_color(CL_Color.new(rest2[0], rest2[1], rest2[2], rest2[3]))
              elsif el2 == "size"
                stroke.set_size(rest2[0])
              end
            }
            @sketch.add_stroke(stroke)
          else
            puts "sketchlayer: Unknonwn: #{el}"
          end
        }
      elsif name == "tilemap"
        width   = get_value_from_tree(["width", "_"],  data, 20)
        height  = get_value_from_tree(["height", "_"], data, 15)
        solid   = get_value_from_tree(["solid", "_"],  data, false)

        tilemap = TilemapLayer.new($tileset, width, height)
        tilemap.set_data(get_value_from_tree(["tiles"], data, []))
        
        if solid then
          @interactive = tilemap
          @width       = width
          @height      = height
        elsif not(@background)
          @background = tilemap
        elsif not(@foreground)
          @foreground = tilemap
        else
          print "Error: Duplicate tilemap in levelfile\n"
        end
      elsif name == "background"
        print "background unhandled\n"
      else
        puts "Creating #{name}..."
        create_gameobject_from_data(@objects, name, data)
      end
    end
    
    print "Tileset: ", $tileset, " ", width, " ", height, "\n"

    if (@background == nil)
      @background = TilemapLayer.new($tileset, @width, @height)
    end

    if (@interactive == nil)
      @interactive = TilemapLayer.new($tileset, @width, @height)
    end
    
    if (@foreground == nil)
      @foreground = TilemapLayer.new($tileset, @width, @height)
    end

    @editormap = EditorMap.new()
    @editormap.add_layer(@background.to_layer()) if @background
    @editormap.add_layer(@interactive.to_layer()) if @interactive
    @editormap.add_layer(@foreground.to_layer()) if @foreground
    @editormap.add_layer(@objects.to_layer())
    @editormap.add_layer(@sketch.to_layer())
    @editormap.set_metadata(self)
  end

  def activate(workspace)
    workspace.set_map(@editormap)
    TilemapLayer.set_current(@interactive)
    ObjectLayer.set_current(@objects)
    connect(@editormap.sig_change(), proc{$gui.on_map_change()})
  end

  def save_tilemap(f, tilemap, name, solid = nil)
    f.write("    (tilemap\n")
    f.write("      (layer  \"%s\")\n" % name)
    f.write("      (solid %s)\n" % if solid == :solid then "#t" else "#f" end)
    f.write("      (speed  %f)\n" % 1.0)
    f.write("      (width  %d)\n" % tilemap.get_width())
    f.write("      (height %d)\n" % tilemap.get_height())
    f.write("      (tiles\n")
    f.write("        ")
    x = 0
    for i in tilemap.get_data()
      f.write("%d " % i)
      x += 1
      if x == width then
        f.write("\n        ")
        x = 0
      end
    end
    f.write("))\n")    
  end

  def save_strokelayer(f, strokelayer)
    f.write("    (strokelayer\n")
    #puts "Strokes: #{strokelayer.get_strokes()} #{strokelayer.get_strokes().size()}"
    strokelayer.get_strokes.each { |stroke|
      f.write("      (stroke\n")
      f.write("        (color "\
              "#{stroke.get_color.get_red()} "\
              "#{stroke.get_color.get_green()} "\
              "#{stroke.get_color.get_blue()} "\
              "#{stroke.get_color.get_alpha()})\n")
      f.write("        (size  #{stroke.get_size})\n")
      stroke.get_dabs.each {|p|
        f.write("        (point #{p.x} #{p.y})\n")
      }
      f.write("  )\n")
    }
    f.write(")\n")
  end


  def save(f)   
    f.write("    (name  \"%s\")\n"  % @name)
    f.write("    (width  %d)\n"  % @width)
    f.write("    (height  %d)\n" % @height)   
    f.write("    (music  \"%s\")\n" % @song)
    f.write("    (gravity %f)\n" % @gravity)
    
    # FIXME: Make me configurable
    f.write("    (background (image \"arctis.jpg\")\n" +
            "                (speed 0.5))\n")
   
    save_tilemap(f, @background,  "background")
    save_tilemap(f, @interactive, "main", :solid)
    save_tilemap(f, @foreground,  "foreground")
    save_strokelayer(f, @sketch)

    f.write("    (camera\n")
    f.write("      (mode \"normal\")\n")
    f.write("      (path\n")

    @objects.get_objects().each {|obj|
      pathnode = obj.get_metadata()
      if (pathnode.is_a?(PathNode))
        f.write("       (point (x %d) (y %d) (speed 1))\n" % obj.get_pos().x, obj.get_pos().y)
      end
    }

    f.write("    ))\n\n")

    for obj in @objects.get_objects()
      # FIXME: not sure why I need get_ruby_object() here
      badguy = get_ruby_object(obj.get_metadata())
      badguy.save(f, obj)
    end
  end
end

# EOF #
