class GameObj
  def property_dialog()
    print "Object ", self.class, " has no properties\n"
  end
end

class SecretArea<GameObj
  attr_accessor :message
  
  def initialize(data, sexpr = [])
    @data    = data

    @message = get_value_from_tree(["message", "_"], sexpr, "")

    x  = get_value_from_tree(["x", "_"],  sexpr, nil)
    y  = get_value_from_tree(["y", "_"],  sexpr, nil)
    width  = get_value_from_tree(["width", "_"],  sexpr, 64)
    height = get_value_from_tree(["height", "_"], sexpr, 64)
    if x != nil and y != nil then
      @data.set_rect(CL_Rect.new(CL_Point.new(x, y), CL_Size.new(width, height)))
    end
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

class AmbientSound<GameObj
  attr_accessor :factor, :bias, :sample, :volume
  
  def initialize(data, sexpr = [])
    @data = data
    @data.set_color(CL_Color.new(200, 200, 200, 128))
    @factor = get_value_from_tree(["distance_factor", "_"],  sexpr, 0.1)
    @bias = get_value_from_tree(["distance_bias", "_"],  sexpr, 200)
    @sample = get_value_from_tree(["sample", "_"],  sexpr, "waterfall")
    @volume = get_value_from_tree(["volume", "_"],  sexpr, 1)

    x  = get_value_from_tree(["x", "_"],  sexpr, nil)
    y  = get_value_from_tree(["y", "_"],  sexpr, nil)
    width  = get_value_from_tree(["width", "_"],  sexpr, 64)
    height = get_value_from_tree(["height", "_"], sexpr, 64)
    if x != nil and y != nil then
      @data.set_rect(CL_Rect.new(CL_Point.new(x, y), CL_Size.new(width, height)))
    end
  end

  def save(f, obj)
    rect = @data.get_rect()
    f.write("        (ambient_sound (x #{rect.left})\n" \
            "                       (y #{rect.top})\n"  \
            "                       (width #{rect.get_width()})\n" \
            "                       (height #{rect.get_height()})\n" \
            "                       (sample #{@sample.inspect})\n" \
	    "                       (distance_factor #{@factor.inspect})\n" \
	    "                       (distance_bias #{@bias.inspect})\n" \
	    "                       (volume #{@volume.inspect}))\n")
  end

  def property_dialog()
    puts @factor.inspect
    puts @bias.inspect
    puts @sample.inspect
    puts @volume.inspect
    dialog = GenericDialog.new("AmbientSound Property Dialog", $gui.get_component())
    dialog.add_float("Distance Factor: ", @factor)
    dialog.add_float("Distance Bias: ", @bias)
    dialog.add_string("Sample: ", @sample)
    dialog.add_int("Max Volume: ",@volume)
    dialog.set_callback(proc{|factor, bias, sample, volume| 
                          @factor = factor
			  @bias = bias
			  @sample = sample
			  @volume = volume
                        })
  end
end

class SequenceTrigger<GameObj
  attr_accessor :sequence, :data
  
  def initialize(data, sexpr = [])
    @data     = data
    @sequence = get_value_from_tree(["sequence", "_"], sexpr, "")
    @data.set_color(CL_Color.new(255, 0, 0, 128))

    x  = get_value_from_tree(["x", "_"],  sexpr, nil)
    y  = get_value_from_tree(["y", "_"],  sexpr, nil)
    width  = get_value_from_tree(["width", "_"],  sexpr, 64)
    height = get_value_from_tree(["height", "_"], sexpr, 64)
    if x != nil and y != nil then
      @data.set_rect(CL_Rect.new(CL_Point.new(x, y), CL_Size.new(width, height)))
    end
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
    dialog = GenericDialog.new("SequenceTrigger Property Dialog", $gui.get_component())
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

class Dispenser<GameObj
  def initialize(data, sexpr = [])
    @data = data
    @badguy = get_value_from_tree(["badguy", "_"], sexpr, "snowball")
    @cycle = get_value_from_tree(["cycle", "_"], sexpr, 2)
  end

  def save(f, obj)
    pos = obj.get_pos()
    f.write("       (dispenser (x %d) (y %d) (badguy \"%s\") (cycle %d))\n" % [pos.x, pos.y, @badguy, @cycle])
  end
  
  def property_dialog()
    dialog = GenericDialog.new("Dispenser Property Dialog", $gui.get_component())
    dialog.add_string("Badguy Type: ", @badguy)
    dialog.add_int("Cycle Type: ", @cycle)
    dialog.set_callback(proc{|badguy, cycle| 
                          @badguy = badguy
                          @cycle = cycle
                        })
  end  
end

class Platform<GameObj
  def initialize(data, sexpr = [])
    @data = data
    @path = get_value_from_tree(["use_path", "_"], sexpr, "path01")
    @type = get_value_from_tree(["type", "_"], sexpr, "flying")
  end

  def save(f, obj)
    pos = obj.get_pos()
    f.write("       (platform (x %d) (y %d) (use_path \"%s\") (type \"%s\"))\n" % [pos.x, pos.y, @path, @type])
  end
  
  def property_dialog()
    dialog = GenericDialog.new("Platform Property Dialog", $gui.get_component())
    dialog.add_string("Use Path: ", @path)
    dialog.add_string("Platform Type: ", @type)
    dialog.set_callback(proc{|path, type| 
                          @path = path
                          @type = type
                        })
  end  
end

class SpawnPoint<GameObj
  attr_accessor :name
  attr_reader   :data

  def initialize(data, sexpr = [])
    @data = data
    @name = get_value_from_tree(["name", "_"],  sexpr, "main")
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

class SimpleObject<GameObj
  def initialize(type)
    @type = type
  end

  def save(f, obj)
    pos = obj.get_pos()
    f.write("       (%s (x %d) (y %d))\n" % [@type, pos.x, pos.y])
  end  
end

class SimpleTileObject<GameObj
  def initialize(data, type, sexpr = [])
    @type = type
    @data = data
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
    f.write("       (%s (x %d) (y %d))\n" % [@type, pos.x, pos.y])
  end  
end

class InfoBlock<GameObj
  attr_accessor :message

  def initialize(data, sexpr = [])
	@data = data
	@message = get_value_from_tree(["message", "_"], sexpr, "")
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
	f.write("      (infoblock (x %d) (y %d)\n" % [pos.x, pos.y]);
	f.write("        (message (_ \"%s\"))\n" % [@message]);
	f.write("      )\n");
  end

  def property_dialog()
    dialog = GenericDialog.new("InfoBox Property Dialog" % [@type],
			$gui.get_component())
    dialog.add_string("Message: ", @message)
    dialog.set_callback(proc{|message| 
                          @message = message
                        })
  end
end 

class Powerup<GameObj
  attr_accessor :sprite

  def initialize(data, sexpr = [])
   @data = data
   @sprite = get_value_from_tree(["sprite", "_"], sexpr, "egg")
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
   f.write("      (powerup (x %d) (y %d)\n" % [pos.x, pos.y]);
   f.write("        (sprite (_ \"%s\"))\n" % [@sprite]);
   f.write("      )\n");
  end

  def property_dialog()
    dialog = GenericDialog.new("Powerup Property Dialog" % [@sprite],
         $gui.get_component())
    dialog.add_string("Sprite: ", @sprite)
    dialog.set_callback(proc{|sprite| 
                          @sprite = sprite
                        })
  end
end 

class ParticleSystem<GameObj
  def initialize(type, sexpr = [])
    @type = type
    @layer = get_value_from_tree(["layer", "_"], sexpr, -1)
  end

  def save(f, obj)
    f.write("       (particles-%s\n" % [@type])
    if(@layer != -1)
      f.write("         (layer %d)\n" % [@layer])
    end
    f.write("       )\n")
  end

  def property_dialog()
    dialog = GenericDialog.new("%s-ParticleSystem Property Dialog" % [@type],
			$gui.get_component())
    dialog.add_int("Layer: ", @layer)
    dialog.set_callback(proc{|layer| 
                          @layer = layer
                        })
  end
end

class Background<GameObj
  def initialize(object, sexpr = [])
    @object = object
    @image = get_value_from_tree(["image", "_"], sexpr, "")
    @speed = get_value_from_tree(["speed", "_"], sexpr, 1.0)
    @layer = get_value_from_tree(["layer", "_"], sexpr, -1)
    @color_top = [0, 0, 0]
    @color_bottom = [0, 0, 0]
    @type = "image"
    if(@image == "" || @type == "gradient")
      if(get_value_from_tree(["top_color"], sexpr, []) != [])
        @color_top = parse_color(
            get_value_from_tree(["top_color"], sexpr, []))
        @color_bottom = parse_color(
            get_value_from_tree(["bottom_color"], sexpr, []))
        @type = "gradient"
      end
    end
    set_icon()
  end

  def parse_color(sexpr = [])
    if(sexpr.size() < 3)
      return [0, 0, 0]
    end
    return [sexpr[0], sexpr[1], sexpr[2]]
  end

  def set_icon()
    if(@type == "image")
      @object.set_sprite(make_sprite($datadir + "images/engine/editor/background.png"))
    else
      @object.set_sprite(make_sprite($datadir + "images/engine/editor/gradient.png"))
    end
  end

  def save(f, obj)
    f.write("       (background\n")
    if(@type == "image")
      f.write("         (image \"%s\")\n" % [@image])
    else
      f.write("         (top_color %d %d %d)\n" % @color_top)
      f.write("         (bottom_color %d %d %d)\n" % @color_bottom)
    end
    f.write("         (speed %f)\n" % [@speed])
    if(@layer != -1)
      f.write("         (layer %d)\n" % [@layer])
    end
    f.write("       )\n")
  end

  def property_dialog()
    dialog = GenericDialog.new("Background Property Dialog", $gui.get_component())
    dialog.add_string("Type: ", @type)
    dialog.add_string("Image: ", @image)
    dialog.add_float("Speed: ", @speed)
    dialog.add_int("Layer: ", @layer)
    dialog.add_int("Top Red: ", @color_top[0])
    dialog.add_int("Top Green: ", @color_top[1])
    dialog.add_int("Top Blue: ", @color_top[2])
    dialog.add_int("Bottom Red: ", @color_bottom[0])
    dialog.add_int("Bottom Green: ", @color_bottom[1])
    dialog.add_int("Bottom Blue: ", @color_bottom[2])
    dialog.set_callback(
          proc{|type, image, speed, layer, topred, topgreen, topblue, botred, botgreen, botblue|
            @type = type
            @image = image
            @speed = speed
            @layer = layer
            @color_top[0] = topred
            @color_top[1] = topgreen
            @color_top[2] = topblue
            @color_bottom[0] = botred
            @color_bottom[1] = botgreen
            @color_bottom[2] = botblue
            set_icon()
            })
  end
end

class UnimplementedObject<GameObj
  def initialize(sexpr = [])
	@sexpr = sexpr
  end

  def save(f)
	f.write("           (sexpr %s)\n" % [@sexpr])
	# TODO
  end
end

class LevelTime<GameObj
  def initialize(sexpr = [])
    @time = get_value_from_tree(["time", "_"], sexpr, 999)
  end

  def save(f, obj)
    f.write("       (leveltime\n");
    f.write("         (time %f)\n" % [@time]);
    f.write("       )\n");
  end

  def property_dialog()
    dialog = GenericDialog.new("LevelTime Property Dialog",
        $gui.get_component())
    dialog.add_float("Time: ", @time)
    dialog.set_callback(proc{|time| 
                          @time = time
                        })
  end
end

class Door<GameObj
  attr_accessor :sector, :spawnpoint
  attr_reader   :data

  def initialize(type, data, sexpr = [])
    @type = type
    @data = data
    @sector     = get_value_from_tree(["sector", "_"], sexpr, "main")
    @spawnpoint = get_value_from_tree(["spawnpoint", "_"], sexpr, "main")

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
    f.write("       (%s\n" % [@type])
    f.write("         (x %d) (y %d)" % [pos.x, pos.y])
    f.write("         (sector \"%s\")\n" % @sector)
    f.write("         (spawnpoint \"%s\")\n" % @spawnpoint)
    f.write("       )\n")
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

class ScriptedObject<GameObj
  def initialize(object, sexpr = [])
    @object = object
    @name = get_value_from_tree(["name", "_"], sexpr, "")
    @sprite = get_value_from_tree(["sprite", "_"], sexpr, "")
    @visible = get_value_from_tree(["visible", "_"], sexpr, true)
    @physic_enabled = get_value_from_tree(["physic-enabled", "_"], sexpr, false)
    @solid = get_value_from_tree(["solid", "_"], sexpr, false)
    @layer = get_value_from_tree(["layer", "_"], sexpr, 100) 
    load_sprite()
  end

  def save(f, obj)
    pos = obj.get_pos()
    f.write("      (scriptedobject\n")
    f.write("        (x %d) (y %d)\n" % [pos.x, pos.y])
    f.write("        (name \"%s\")\n" % @name)
    f.write("        (sprite \"%s\")\n" % @sprite)
    f.write("        (layer %d)\n" % @layer)
    if(@visible == true)
      f.write("        (visible #t)\n")
    else
      f.write("        (visible #f)\n")
    end
    if(@physic_enabled == true)
      f.write("        (physic-enabled #t)\n")
    else
      f.write("        (physic-enabled #f)\n")
    end
    if(@solid == true)
      f.write("        (solid #t)\n")
    else
      f.write("        (solid #f)\n")
    end
    f.write("      )\n");
  end

  def load_sprite()
    begin
      sprite = load_cl_sprite($datadir + @sprite)
      @object.set_sprite(sprite)

    rescue
      sprite = load_cl_sprite($datadir + "images/engine/editor/scriptedobject.png")
    end
    @object.set_sprite(sprite)
  end

  def property_dialog()
    dialog = GenericDialog.new("Scripted Object Property Dialog",
                               $gui.get_component())
    dialog.add_string("Name: ", @name)
    dialog.add_string("Sprite: ", @sprite)
    dialog.add_int("Layer: ", @layer)
    dialog.add_bool("Visible: ", @visible)
    dialog.add_bool("Physics: ", @physic_enabled)
    dialog.add_bool("Solid: ", @solid)
    dialog.set_callback(proc{|name, sprite, layer, visible, physic_enabled, solid| 
                          @name = name
                          @sprite = sprite
                          @layer = layer
                          @visible = visible
                          @physic_enabled = physic_enabled
                          @solid = solid
                          load_sprite()
                        })
  end
end

# EOF #
