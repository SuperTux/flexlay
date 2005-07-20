
class WorldmapObject
  attr_accessor :obj

  def initialize()
    @obj = nil
  end
end

class WMSpawnPoint<WorldmapObject
  def initialize()
    @name = ""
    @obj = ObjMapSpriteObject.new(
        make_sprite($datadir + "images/worldmap/common/tux.png"),
        CL_Pointf.new(0, 0), make_metadata(self))
     connect_v1_ObjMapObject(@obj.to_object.sig_move(), method(:on_move))
  end

  def on_move(data)
    pos = @obj.to_object.get_pos()
    pos.x = (((pos.x+16)/32).to_i)*32                               
    pos.y = (((pos.y+16)/32).to_i)*32
    @obj.to_object.set_pos(pos)
  end

  def parse(data)
    x = get_value_from_tree(["x", "_"], data, 0)
    y = get_value_from_tree(["y", "_"], data, 0)
    @obj.to_object.set_pos(CL_Pointf.new(x * 32, y * 32))
    @name = get_value_from_tree(["name", "_"], data, "")
  end
                                                                               
  def save(writer)
    writer.start_list("spawnpoint")
    pos = @obj.to_object.get_pos()
    writer.write_int("x", pos.x / 32)
    writer.write_int("y", pos.y / 32)
    writer.write_string("name", @name)
    writer.end_list("level")
  end

  def property_dialog()
    dialog = GenericDialog.new("SpawnPoint Property Dialog",
        $gui.get_component())
    dialog.add_string("Name", @name)
    dialog.set_callback(proc{|name|
          @name = name
      })                                                          
  end
end

class WorldmapLevel<WorldmapObject
  def initialize()
    @name = ""
    @extro_filename = ""
    @quit_worldmap = false
    @obj = ObjMapSpriteObject.new(
            make_sprite($datadir + "images/worldmap/common/leveldot_green.png"),
            CL_Pointf.new(0, 0), make_metadata(self))
    connect_v1_ObjMapObject(@obj.to_object.sig_move(), method(:on_move))
  end

  def parse(data)
    x = get_value_from_tree(["x", "_"], data, 0)
    y = get_value_from_tree(["y", "_"], data, 0)
    @obj.to_object.set_pos(CL_Pointf.new(x * 32, y * 32))
    @name = get_value_from_tree(["name", "_"], data, "")
    @extro_filename = get_value_from_tree(["extro-filename", "_"], data, "")
    @quit_worldmap = get_value_from_tree(["quit-worldmap", "_"], data, false)
  end

  def save(writer)
    writer.start_list("level")
    pos = @obj.to_object.get_pos()
    writer.write_int("x", pos.x / 32)
    writer.write_int("y", pos.y / 32)
    writer.write_string("name", @name)
    if @extro_filename != ""
      writer.write_string("extro-filename", @extro_filename)
    end
    if @quit_worldmap == true
      writer.write_bool("quit-worldmap", @quit_worldmap)
    end
    writer.end_list("level")
  end

  def on_move(data)
    pos = @obj.to_object.get_pos()
    pos.x = (((pos.x+16)/32).to_i)*32
    pos.y = (((pos.y+16)/32).to_i)*32
    @obj.to_object.set_pos(pos)
  end

  def property_dialog()
    dialog = GenericDialog.new("LevelTile Property Dialog",
        $gui.get_component())
    dialog.add_string("level", @name)
    dialog.add_string("extro-filename", @extro_filename)
    dialog.add_bool("quit-worldmap", @quit_worldmap)
    dialog.set_callback(proc{|name, extro_filename, quit_worldmap|
          @name = name
          @extro_filename = extro_filename
          @quit_worldmap = quit_worldmap
        })
  end
end

class SpecialTile<WorldmapObject
  def initialize()
    @map_message = ""
    @apply_to_direction = ""
    @passive_message = false
    @teleport_x = 0
    @teleport_y = 0
    @invisible_tile = false
    @obj = ObjMapSpriteObject.new(
            make_sprite($datadir + "images/worldmap/common/teleporterdot.png"),
            CL_Pointf.new(0, 0), make_metadata(self))
    connect_v1_ObjMapObject(@obj.to_object.sig_move(), method(:on_move))
  end

  def parse(data)
    x = get_value_from_tree(["x", "_"], data, 0)
    y = get_value_from_tree(["y", "_"], data, 0)
    @obj.to_object.set_pos(CL_Pointf.new(x * 32, y * 32))
    @map_message = get_value_from_tree(["map-message", "_"], data, "")
    @passive_message = get_value_from_tree(["passive-message", "_"], data, false)
    @teleport_x = get_value_from_tree(["teleport-to-x", "_"], data, -1)
    @teleport_y = get_value_from_tree(["teleport-to-y", "_"], data, -1)
    @invisible_tile = get_value_from_tree(["invisible_tile", "_"], data, false)
    @apply_to_direction = get_value_from_tree(["apply-to-direction", "_"],
        data, "")
  end

  def save(writer)
    writer.start_list("special-tile")
    pos = @obj.to_object.get_pos()
    writer.write_int("x", pos.x / 32)
    writer.write_int("y", pos.y / 32)
    if(@map_message != "")
      writer.write_string("map-message", @map_message, true)
    end
    if(@passive_message != false)
      writer.write_bool("passive-message", @passive_message)
    end
    if(@invisible_tile != false)
      writer.write_bool("invisible-tile", @invisible_tile)
    end
    if(@apply_to_direction != "")
      writer.write_string("apply-to-direction", @apply_to_direction)
    end
    if(@teleport_x != -1)
      writer.write_int("teleport-to-x", @teleport_x)
      writer.write_int("teleport-to-y", @teleport_y)
    end
    writer.end_list("special-tile")
  end

  def on_move(data)
    pos = @obj.to_object.get_pos()
    pos.x = (((pos.x+16)/32).to_i)*32
    pos.y = (((pos.y+16)/32).to_i)*32
    @obj.to_object.set_pos(pos)
  end                                  

  def property_dialog()
    dialog = GenericDialog.new("SpecialTile Property Dialog",
        $gui.get_component())
    dialog.add_string("map-message", @map_message)
    dialog.add_bool("passive-message", @passive_message)
    dialog.add_bool("invisible-tile", @invisible_tile)
    dialog.add_string("apply-to-direction", @apply_to_direction)
    dialog.add_int("teleport-to-x", @teleport_x)
    dialog.add_int("teleport-to-y", @teleport_y)
    
    dialog.set_callback(proc{|map_message, passive_message, invisible_tile,
        apply_to_direction, teleport_x, teleport_y|
          @map_message = map_message
          @passive_message = passive_message
          @invisible_tile = invisible_tile
          @apply_to_direction = apply_to_direction
          @teleport_x = teleport_x
          @teleport_y = teleport_y
        })
  end
end

$worldmap_objects = [
  ["level", "images/worldmap/common/leveldot_green.png", WorldmapLevel],
  ["special-tile", "images/worldmap/common/teleporterdot.png", SpecialTile],
  ["spawnpoint", "images/worldmap/common/tux.png", WMSpawnPoint],
]

def create_worldmapobject_at_pos(objmap, name, pos)
  objectclass = $worldmap_objects.find {|x| x[0] == name}
  if objectclass == nil 
    print "Error: Couldn't resolve object type: " , name, "\n"  
    return
  end

  (name, image, _class) = objectclass
  object = _class.new()
  object.obj.to_object.set_pos(pos)
  cmd = ObjectAddCommand.new(objmap)
  cmd.add_object(object.obj.to_object);
  $gui.workspace.get_map().execute(cmd.to_command());
  return object
end

def create_worldmapobject_from_data(objmap, name, sexpr)
  objectclass = $worldmap_objects.find {|x| x[0] == name}
  if objectclass == nil 
    print "Error: Couldn't resolve object type: " , name, "\n"
    return
  end

  (name, image, _class) = objectclass
  object = _class.new()
  object.parse(sexpr)
  cmd = ObjectAddCommand.new(objmap)
  cmd.add_object(object.obj.to_object);
  $gui.workspace.get_map().execute(cmd.to_command());
  return object
end

