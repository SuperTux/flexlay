class Level
  attr_reader :interactive

  def initialize(width, height)
    @interactive  = TilemapLayer.new($tileset, width, height)
    @objects = ObjectLayer.new()

    @layers = []
    @layers.push(@interactive)
    @layers.push(@objects)

    @editormap = EditorMap.new()
    @layers.each {|layer| @editormap.add_layer(layer.to_layer()) }
    @editormap.set_data(self)
    @current_layer = 0
  end

  def activate(workspace)
    $gui.workspace.set_map(@editormap)

    TilemapLayer.set_current(@layers[@current_layer])
    ObjectLayer.set_current(@objects)
    
    connect(@editormap.sig_change(), proc{
              $gui.on_map_change()
            })
  end

end

# EOF #
