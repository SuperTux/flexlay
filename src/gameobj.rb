class GameObj
  def property_dialog()
    print "No PropertyDialog Implemented for type: ", self.class(), "\n"
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

  def initialize()
    @name = "start"
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

  def initialize(data = [])
    @sector     = get_value_from_tree(["sector", "_"], data, "main")
    @spawnpoint = get_value_from_tree(["spawnpoint", "_"], data, "start")
  end

  def save(f, obj)
    pos = obj.get_pos()
    f.write("       (door\n")
    f.write("         (x %d) (y %d)" % [pos.x, pos.y])
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
