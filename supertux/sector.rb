class Sector
  @parent    = nil
  @name      = nil
  @music     = nil
  @gravity   = 10.0
  
  @width  = nil
  @height = nil
  
  @background  = nil
  @interactive = nil
  @foreground  = nil
  
  @objects	  = nil
#  sketch    = nil
  @editormap = nil

  @cameramode = "normal"

  attr_reader   :objects, :background, :interactive, :foreground, :parent, :width, :height
  attr_accessor :name, :music, :gravity
  
  def initialize(parent)
    @parent = parent
    @cameramode = "normal"
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
    @music = ""
    @gravity = 10.0
    
    @width  = width
    @height = height
    
    @foreground  = TilemapLayer.new($tileset, @width, @height)
    @interactive = TilemapLayer.new($tileset, @width, @height)
    @background  = TilemapLayer.new($tileset, @width, @height)       
    @objects	 = ObjectLayer.new()
    # @sketch  = SketchLayer.new()

    @editormap = EditorMap.new()
#    @editormap.set_background_color(CL_Color.new(255, 255, 255))
    @editormap.add_layer(@background.to_layer())
    @editormap.add_layer(@interactive.to_layer())
    @editormap.add_layer(@objects.to_layer())
    @editormap.add_layer(@foreground.to_layer())
#    @editormap.add_layer(@sketch.to_layer())
    # FIXME: Data might not get freed since its 'recursively' refcounted
    @editormap.set_metadata(self)
    return self
  end

  def load_v1(data)
    @name = "main"
    @music = get_value_from_tree(["music", "_"], data, "")
    @gravity = get_value_from_tree(["gravity", "_"], data, 10.0)
    
    @width  = get_value_from_tree(["width", "_"], data, 20)
    @height = get_value_from_tree(["height", "_"], data, 15)
    
    @foreground  = TilemapLayer.new($tileset, @width, @height)
    @foreground.set_data(get_value_from_tree(["foreground-tm"], data, []))
    
    @interactive = TilemapLayer.new($tileset, @width, @height)
    @interactive.set_data(get_value_from_tree(["interactive-tm"], data, []))
    
    @background  = TilemapLayer.new($tileset, @width, @height)
    @background.set_data(get_value_from_tree(["background-tm"], data, []))

	@cameramode = "normal"

    @objects = ObjectLayer.new()
    for i in get_value_from_tree(["objects"], data, [])
      (name, odata) = i[0], i[1..-1]
      # fix some old object names
      if(name == "money")
        name = "jumpy"
      end
      if(name == "laptop")
        name = "mriceblock"
      end
      create_gameobject_from_data(@objects, name, odata)
    end

	start_pos_x = get_value_from_tree(["start_pos_x", "_"], data, 0)
	start_pos_y = get_value_from_tree(["start_pos_y", "_"], data, 0)
	sexpr = [["name", "main"], ["x", start_pos_x], ["y", start_pos_y]]
	create_gameobject_from_data(@objects, "spawnpoint", sexpr)

	background = get_value_from_tree(["background", "_"], data, "")
	if(background != "")
	  sexpr = [["image", background], ["speed", 0.5]]
	  create_gameobject_from_data(@objects, "background", sexpr)
	end

	partsys = get_value_from_tree(["particle_system", "_"], data, "")
	if(partsys == "snow")
	  sexpr = []
	  create_gameobject_from_data(@objects, "particles-snow", sexpr)
	elsif(partsys == "rain")
	  sexpr = []
	  create_gameobject_from_data(@objects, "particles-rain", sexpr)
	elsif(partsys == "clouds")
	  sexpr = []
	  create_gameobject_from_data(@objects, "particles-clouds", sexpr)
	elsif(partsys == "")
	elsif
	  print "Unknown particle system type '", partsys, "'\n"
	end
	    
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
    @music = ""
    @gravity = 10.0
	@cameramode = "normal"
    
    @width  = 0
    @height = 0

    @background  = nil
    @interactive = nil
    @foreground  = nil
    
    @objects = ObjectLayer.new()
#    @sketch = SketchLayer.new()

    for i in data
      (name,data) = i[0], i[1..-1]
      if name == "name"
        @name = data[0]
      elsif name == "gravity"
        @gravity = data[0]
	  elsif name == "music"
		@music = data[0]
      elsif name == "tilemap"
		layer   = get_value_from_tree(["layer", "_"], data, "interactive")
        width   = get_value_from_tree(["width", "_"],  data, 20)
        height  = get_value_from_tree(["height", "_"], data, 15)
        solid   = get_value_from_tree(["solid", "_"],  data, false)

        tilemap = TilemapLayer.new($tileset, width, height)
        tilemap.set_data(get_value_from_tree(["tiles"], data, []))
        
        if solid then
          @interactive = tilemap
          @width       = width
          @height      = height
        elsif layer == "background"
          @background = tilemap
        elsif layer == "foreground"
		  @foreground = tilemap
        else
          print "Flexlay doesn't handle tilemap layer '", layer, "'.\n"
        end
	  elsif name == "camera"
		@cameramode = "normal"
		# TODO...
      else
        puts "Creating #{name}..."
		create_gameobject_from_data(@objects, name, data)
      end
    end
    
    print "Tileset: ", $tileset, " ", width, " ", height, "\n"

	if(@interactive == nil || @width == 0 || @height == 0)
	  throw "No interactive tilemap in sector '", @name , "'.\n"
	end

    if (@background == nil)
      @background = TilemapLayer.new($tileset, @width, @height)
    end

    if (@foreground == nil)
      @foreground = TilemapLayer.new($tileset, @width, @height)
    end

    @editormap = EditorMap.new()
    @editormap.add_layer(@background.to_layer()) if @background
    @editormap.add_layer(@interactive.to_layer()) if @interactive
    @editormap.add_layer(@foreground.to_layer()) if @foreground
    @editormap.add_layer(@objects.to_layer())
#    @editormap.add_layer(@sketch.to_layer())
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

  def save(f)   
    f.write("    (name  \"%s\")\n"  % @name)
    if(@music != "")
        f.write("    (music  \"%s\")\n" % @music)
    end
    f.write("    (gravity %f)\n" % @gravity)
    
    # FIXME: Make me configurable
    f.write("    (background (image \"arctis.jpg\")\n" +
            "                (speed 0.5))\n")
   
    save_tilemap(f, @background,  "background")
    save_tilemap(f, @interactive, "interactive", :solid)
    save_tilemap(f, @foreground,  "foreground")
#    save_strokelayer(f, @sketch)

    f.write("    (camera\n")
    f.write("      (mode \"%s\")\n" % [@cameramode])
#    f.write("      (path\n")
#    @objects.get_objects().each {|obj|
#      pathnode = obj.get_metadata()
#      if (pathnode.is_a?(PathNode))
#        f.write("       (point (x %d) (y %d) (speed 1))\n" % obj.get_pos().x, obj.get_pos().y)
#      end
#    }
#    f.write("      )")
	f.write("    )\n\n")

    for obj in @objects.get_objects()
      # FIXME: not sure why I need get_ruby_object() here
      object = get_ruby_object(obj.get_metadata())
      object.save(f, obj)
    end
  end
end

# EOF #
