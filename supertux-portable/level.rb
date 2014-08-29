class Level
  attr_accessor :interactive, :objects, :editormap, :layers, :current_layer

  def initialize()
    @interactive = nil
    @objects     = nil
    @editormap   = nil
    @layers      = []
    @current_layer = 0
  end

  def Level.new_from_size(width, height)
    level = Level.new()

    level.interactive  = TilemapLayer.new($tileset, width, height)
    level.objects = ObjectLayer.new()

    level.layers = []
    level.layers.push(level.interactive)
    level.layers.push(level.objects)

    level.editormap = EditorMap.new()
    level.layers.each {|layer| level.editormap.add_layer(layer.to_layer()) }
    level.editormap.set_data(level)
    level.current_layer = 0
    
    return level
  end

  def Level.new_from_file(filename)
    f = File.new(filename, "r")
    (width, height, *data) = f.read().unpack("SSS*")
    f.close()

    level = Level.new()
    level.interactive  = TilemapLayer.new($tileset, width, height)
    level.interactive.set_data(data)
    level.objects = ObjectLayer.new()

    level.layers = []
    level.layers.push(level.interactive)
    level.layers.push(level.objects)

    level.editormap = EditorMap.new()
    level.layers.each {|layer| level.editormap.add_layer(layer.to_layer()) }
    level.editormap.set_data(level)
    level.current_layer = 0

    return level
  end

  def activate(workspace)
    $gui.workspace.set_map(@editormap)

    TilemapLayer.set_current(@layers[@current_layer])
    ObjectLayer.set_current(@objects)
    
    connect(@editormap.sig_change(), proc{
              $gui.on_map_change()
            })
  end

  def save(filename)
    f = File.new(filename, "w")
    f.write([@interactive.get_width, @interactive.get_height, *@interactive.get_data()].pack("SSS*"))
    f.close()
  end
end

# EOF #
