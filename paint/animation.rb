class Image
  # OnionSkinLayer.new(get_current_image().editormap.get_layer(0)).update()  
  def add_onion_skin(onion_skin)
    if not @onion_skin then
      @editormap.add_layer(onion_skin.to_layer(), 0)
      @onion_skin = onion_skin
    else
      puts "Already have onion skin!!"
    end
  end

  def update_onion_skin()
    if @onion_skin then
      @onion_skin.update()
    end
  end
end

class Animation
  def initialize()
    @frames = [Image.new()]
    @current_frame = 0
  end

  def next_frame()
    @current_frame = (@current_frame + 1) % @frames.length
    get_current_image().activate($gui.workspace)
    get_current_image().update_onion_skin()
  end

  def previous_frame()
    @current_frame = (@current_frame - 1) % @frames.length
    get_current_image().activate($gui.workspace)
    get_current_image().update_onion_skin()
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
      img.add_onion_skin(onion_skin)
      
      if (@frames.length > 1) then
        onion_skin.add_map(@frames[(@current_frame - 1)%@frames.length].editormap, 
                           CL_Color.new(255, 255, 255, 150))
      end

      if (@frames.length > 2) then
        onion_skin.add_map(@frames[(@current_frame - 2)%@frames.length].editormap, 
                           CL_Color.new(255, 255, 255, 75))
      end      
      
      img.update_onion_skin()
    end
  end
end

# EOF #
