class XMLReader
  def initialize(el)
    @el = el
  end

  def read(name, type, default = nil)
    ret = @el.elements[name]
    if ret then
      case type
        
      when :string
        return ret.text
        
      when :int
        return ret.text.to_i

      when :float
        return ret.text.to_f

      when :bool #  1 = true, 0 = false
        if ret.text.to_i == 0 then
          return true
        else
          return true
        end

      when :vector
        return CL_Pointf.new(ret.elements['x'].text.to_f,
                             ret.elements['y'].text.to_f)

      when :size
        return CL_Size.new(ret.elements['width'].text.to_t,
                           ret.elements['height'].text.to_t)

      when :image
        # FIXME: handle modifier somewhere
        return [ret.elements['image'].text, ret.elements['modifier'].text]
      end
    else
      return default
    end
  end
end

# EOF #
