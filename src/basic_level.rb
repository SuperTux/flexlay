class Level
  ## Level Properties
  attr_reader :name
  attr_writer :name

  def initialize(*params)
    if params.length() == 2 then
      # New Level
      (width, height) = params

      @width  = width
      @height = height

      @foreground  = TilemapLayer.new($tileset, @width, @height)
      @interactive = TilemapLayer.new($tileset, @width, @height)
      @background  = TilemapLayer.new($tileset, @width, @height)
      @objects = ObjectLayer.new()

      @editormap = EditorMap.new()
      @editormap.add_layer(@background.to_layer())
      @editormap.add_layer(@interactive.to_layer())
      @editormap.add_layer(@objects.to_layer())
      @editormap.add_layer(@foreground.to_layer())
      
      # FIXME: Data might not get freed since its 'recursively' refcounted
      @editormap.set_metadata(make_metadata(self))
    end
  end

  def activate(workspace)
    workspace.set_map(@editormap)

    TilemapLayer.set_current(@interactive)
    ObjectLayer.set_current(@objects)
  end

  def load(filename)
    print "Load Level to '", filename, "'\n"
    ## Insert your load code here
  end

  def save(filename)
    print "Save Level to '", filename, "'\n"
    ## Insert your save code here
  end
end

# EOF #
