class WorldMap
  attr_reader :name, :music, :intro_filename, :start_pos_x, :start_pos_y, :filename
  attr_writer :name, :music, :intro_filename, :start_pos_x, :start_pos_y, :filename
  attr_accessor :objects
  
  def initialize(*params)
    @name     = "No Name"
    @music    = ""
    @intro_filename = ""
    @start_pos_x = 0
    @start_pos_y = 0
    @filename = nil
    @width = 0
    @height = 0
    @objects = ObjectLayer.new()

    if params.length() == 2 then
      # New Level
      (width, height) = params
      
      @width  = width
      @height = height
      @tilemap = TileMap.new()
      @tilemap.new_from_size(@width, @height)
    elsif params.length() == 1 then
      # Load Level from file
      (@filename,) = params
      
      tree = load_lisp(@filename, :"supertux-worldmap")
      
      data = tree[1..-1]
      parse(data)
    else
      raise "Wrong arguments for SuperTux::___init__"
    end
  end
  
  def parse(data)
    for i in data
      (name, data) = i[0], i[1..-1]

      if name == :properties
        @name = get_value_from_tree(["name", "_"], data, "No Name")
        print "Name:" + @name.to_s
        @music = get_value_from_tree(["music", "_"], data, "salcon.ogg")
        @intro_filename = get_value_from_tree(["intro-filename", "_"], data, "")
        @start_pos_x = get_value_from_tree(["start_pos_x", "_"], data, 0)
        @start_pos_y = get_value_from_tree(["start_pos_y", "_"], data, 0)
      elsif name == :tilemap
        @tilemap = TileMap.new()
        @tilemap.parse(data)
      else
        create_worldmapobject_from_data(@objects, name, data)
      end
    end
  end

  def save(filename)
    f = File.new(filename, "w")
    writer = LispWriter.new(f)

    writer.write_comment("Generated by Flexlay Editor")
    writer.start_list("supertux-worldmap")

    writer.start_list("properties")
    writer.write_string("name", @name, true)
    if @intro_filename != ""
      writer.write_string("intro-filename", @intro_filename)
    end
    writer.write_string("music", @music)
    writer.write_int("start_pos_x", @start_pos_x)
    writer.write_int("start_pos_y", @start_pos_y)
    writer.end_list("properties")

    @tilemap.save(writer)

    for o in @objects.get_objects()
      object = o.get_data()
      object.save(writer)
    end
    writer.end_list("supertux-worldmap")
    f.close()
  end
  
  def activate(workspace)
    @editormap = EditorMap.new()
    @editormap.add_layer(@tilemap.tilemaplayer.to_layer())
    @editormap.add_layer(@objects.to_layer())
    @editormap.set_metadata(self)

    workspace.set_map(@editormap)
    TilemapLayer.set_current(@tilemap.tilemaplayer)
    ObjectLayer.set_current(@objects)
    connect(@editormap.sig_change(), proc{$gui.on_map_change()})
  end
end

