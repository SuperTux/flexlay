class GameObj
  def property_dialog()
    print "No PropertyDialog Implemented for type: ", self.class(), "\n"
  end
end

class SecretArea<GameObj
  attr_accessor :message
  
  def initialize(data, sexpr = [])
    @data    = data

    @message = get_value_from_tree(["message", "_"], sexpr, "")

    x  = get_value_from_tree(["x", "_"],  sexpr, 0)
    y  = get_value_from_tree(["y", "_"],  sexpr, 0)
    width  = get_value_from_tree(["width", "_"],  sexpr, 64)
    height = get_value_from_tree(["height", "_"], sexpr, 64)
    @data.set_rect(CL_Rect.new(CL_Point.new(x, y), CL_Size.new(width, height)))
  end

  def save(f, obj)
    rect = @data.get_rect()
    f.write("        (secretarea (x #{rect.left})\n" \
            "                    (y #{rect.top})\n"  \
            "                    (width #{rect.get_width()})\n" \
            "                    (height #{rect.get_height()})\n" \
            "                    (message #{@message.inspect}))\n")
  end

  def property_dialog()
    puts @message.inspect
    dialog = GenericDialog.new("SecretArea Property Dialog", $gui.get_component())
    dialog.add_string("Message: ", @message)
    dialog.set_callback(proc{|message| 
                          @message = message
                        })
  end
end

class SequenceTrigger<GameObj
  attr_accessor :sequence, :data
  
  def initialize(data, sexpr = [])
    @data     = data
    @sequence = get_value_from_tree(["sequence", "_"], sexpr, "")
    @data.set_color(CL_Color.new(255, 0, 0, 128))
    

    x  = get_value_from_tree(["x", "_"],  sexpr, 0)
    y  = get_value_from_tree(["y", "_"],  sexpr, 0)
    width  = get_value_from_tree(["width", "_"],  sexpr, 64)
    height = get_value_from_tree(["height", "_"], sexpr, 64)
    @data.set_rect(CL_Rect.new(CL_Point.new(x, y), CL_Size.new(width, height)))
  end

  def save(f, obj)
    rect = @data.get_rect()
    f.write("        (sequencetrigger (x #{rect.left})\n" \
            "                         (y #{rect.top})\n"  \
            "                         (width #{rect.get_width()})\n" \
            "                         (height #{rect.get_height()})\n" \
            "                         (sequence #{@sequence.inspect}))\n")
  end

  def property_dialog()
    puts @sequence.inspect
    dialog = GenericDialog.new("SecretArea Property Dialog", $gui.get_component())
    dialog.add_string("Sequence: ", @sequence)
    dialog.set_callback(proc{|sequence| 
                          @sequence = sequence
                        })
  end
end

class BadGuy<GameObj
  def initialize(type)
    @type = type
  end

  def save(f, obj)
    pos = obj.get_pos()
    f.write("       (%s (x %d) (y %d))\n" % [@type, pos.x, pos.y])
  end  
end

class SpawnPoint<GameObj
  attr_accessor :name
  attr_reader   :data

  def initialize(data)
    @data = data
    @name = "start"
    connect_v1_ObjMapObject(data.to_object.sig_move(), method(:on_move))
    on_move(data)
  end

  def on_move(data)
    pos = @data.to_object.get_pos()
    pos.x = (((pos.x+16)/32).to_i)*32
    pos.y = (((pos.y+16)/32).to_i)*32
    @data.to_object.set_pos(pos)
  end

  def save(f, obj)
    pos = obj.get_pos()
    f.write("       (spawnpoint (name \"%s\") (x %d) (y %d))\n" % [@name, pos.x, pos.y])
  end

  def property_dialog()
    dialog = GenericDialog.new("SpawnPoint Property Dialog", $gui.get_component())
    dialog.add_string("Name: ", @name)
    dialog.set_callback(proc{|name| 
                          @name = name
                        })
  end
end

class Door<GameObj
  attr_accessor :sector, :spawnpoint
  attr_reader   :data

  def initialize(data, sexpr = [])
    @data = data
    @sector     = get_value_from_tree(["sector", "_"], sexpr, "main")
    @spawnpoint = get_value_from_tree(["spawnpoint", "_"], sexpr, "start")

    connect_v1_ObjMapObject(@data.to_object.sig_move(), method(:on_move))
    on_move(data)
  end

  def on_move(data)
    pos = @data.to_object.get_pos()
    pos.x = (((pos.x+16)/32).to_i)*32
    pos.y = (((pos.y+16)/32).to_i)*32
    @data.to_object.set_pos(pos)
  end

  def save(f, obj)
    pos = obj.get_pos()
    f.write("       (door\n")
    f.write("         (x %d) (y %d)" % [pos.x, pos.y])
    # FIXME: not so sure if width/height make sense
    f.write("         (width  32)\n")
    f.write("         (height 64)\n")
    f.write("         (sector \"%s\")\n" % @sector)
    f.write("         (spawnpoint \"%s\")\n" % @spawnpoint)
    f.write("         )\n")
  end  

  def property_dialog()
    dialog = GenericDialog.new("Door Property Dialog", $gui.get_component())
    dialog.add_string("Sector: ", @sector)
    dialog.add_string("Spawnpoint: ", @spawnpoint)
    dialog.set_callback(proc{|sector, spawnpoint| 
                          @sector = sector
                          @spawnpoint = spawnpoint
                        })
  end
end

class PathNode<GameObj
  attr_accessor :node
  
  def initialize(node)
    @node = node
  end

  def save(f, obj)
  end
end


# EOF #
