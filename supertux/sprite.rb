class Sprite
  def initialize(filename)
    @actions = Hash.new()
  
    tree = sexpr_read_from_file(filename)
    if tree == nil then
      raise "Error: Couldn't load: '#{filename}'"
    end

    @basedir = File.dirname(filename) + "/"
    
    tree[1..-1].each do |i|
      case i[0]
        when :action
          action = Action.new()
          action.parse(i[1..-1])
          @actions[action.name] = action
          if @actions.default == nil || action.name == "default":
            @actions.default = action
          end
        else
          print "Unknown symbol '#{i[0]}' in sprite '#{filename}'"
      end
    end
  end

  class Action
    attr_accessor :name, :image, :x_offset, :y_offset
  
    def parse(sexpr)
      @name = get_value_from_tree(["name", "_"], sexpr, "default")
      @x_offset = get_value_from_tree(["x-offset", "_"], sexpr, 0)
      @y_offset = get_value_from_tree(["y-offset", "_"], sexpr, 0)
      # we only parse the first image for now as we don't have support for
      # animation in flexlay anyway
      @image = get_value_from_tree(["images", "_"], sexpr, 0)
    end
  end

  def get_cl_sprite(action = "default")
    action = @actions[action]
    sprite = make_sprite(@basedir + action.image)
    sprite.set_frame_offset(0, CL_Point.new(action.x_offset, action.y_offset))
    return sprite
  end
end
