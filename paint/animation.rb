class Animation
  def initialize()
    @frames = [Image.new()]
    @current_frame = 0
  end

  def next_frame()
    @current_frame = (@current_frame + 1) % @frames.length
    get_current_image().activate($gui.workspace)
  end

  def previous_frame()
    @current_frame = (@current_frame - 1) % @frames.length
    get_current_image().activate($gui.workspace)
  end

  def get_current_image()
    return @frames[@current_frame]
  end

  def goto_frame(index)
    @current_frame = index % @frames.length
  end

  def add_frame(index = nil)
    if index then
      
    else
      img = Image.new()
      @frames.push(img)
      next_frame()

      # Hack for Onion_Skin
      onion_skin = OnionSkinLayer.new(1024, 768)
      # onion_skin.to_layer().set_pos(CL_Pointf.new(-100, -100))
      img.editormap.add_layer(onion_skin.to_layer(), 0)
      
      if (@current_frame > 0) then
        onion_skin.add_map(@frames[@current_frame - 1].editormap, 0.5)
      end
    end
  end
end

# EOF #
