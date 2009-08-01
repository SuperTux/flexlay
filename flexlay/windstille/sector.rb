##  $Id$
## 
##  Flexlay - A Generic 2D Game Editor
##  Copyright (C) 2004 Ingo Ruhnke <grumbel@gmx.de>
##
##  This program is free software; you can redistribute it and/or
##  modify it under the terms of the GNU General Public License
##  as published by the Free Software Foundation; either version 2
##  of the License, or (at your option) any later version.
##
##  This program is distributed in the hope that it will be useful,
##  but WITHOUT ANY WARRANTY; without even the implied warranty of
##  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
##  GNU General Public License for more details.
## 
##  You should have received a copy of the GNU General Public License
##  along with this program; if not, write to the Free Software
##  Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.

class TilemapMetadata
  attr_accessor :name, :x_offset, :y_offset, :z_pos
  
  def initialize(params = {})
    @name     = (params[:name] or "")
    @x_offset = (params[:x_offset] or 0)
    @y_offset = (params[:y_offset] or 0)
    @z_pos    = (params[:z_pos] or 0)
  end
end

class Sector
  ## Sector Properties
  attr_reader :layers, :editormap
  attr_reader :background, :interactive, :interactivebackground,:foreground, :objects

  attr_accessor :name

  def initialize(*params)
    if params.length() == 2 then
      initialize_new(*params)
    else 
      initialize_from_file(*params)
    end
  end

  def initialize_new(width, height)
    # New Sector
    @current_layer = 1
    
    @background  = TilemapLayer.new($tileset, width, height)
    @interactive = TilemapLayer.new($tileset, width, height)
    @interactivebackground = TilemapLayer.new($tileset, width, height)
    @foreground  = TilemapLayer.new($tileset, width, height)
    
    @background.set_metadata(TilemapMetadata.new(:name =>  'background'))
    @foreground.set_metadata(TilemapMetadata.new(:name =>  'foreground'))
    @interactive.set_metadata(TilemapMetadata.new(:name => 'interactive'))
    @interactive.set_metadata(TilemapMetadata.new(:name => 'interactivebackground'))

    @layers = []
    @layers += [@background]
    @layers += [@interactivebackground]
    @layers += [@interactive]
    @layers += [@foreground]
    @layers += [@objects = ObjectLayer.new()]

    @editormap = EditorMap.new(true)

    @layers.each {|layer| @editormap.add_layer(layer.to_layer()) }
    
    # FIXME: Data might not get freed since its 'recursively' refcounted
    @editormap.set_data(self)
  end

  def initialize_from_file(filename)
    tree = sexpr_read_from_file(filename)
    if tree == nil
      raise("Couldn't load level: %s" % filename)
    end
    
    data = tree[1..-1]
    
    @version = get_value_from_tree(["version", "_"], data, 0)
    @name    = get_value_from_tree(["name", "_"],    data, 0)
    @music   = get_value_from_tree(["music", "_"],   data, 0)

    objects = get_value_from_tree(["objects"],  data, [])
    
    @layers = []
    @objects = ObjectLayer.new()

    objects.each{ |object|
      objtype = object[0]
      objdata = object[1..-1]

      case objtype
      when :tilemap
        name   = get_value_from_tree(["name",  "_"],   objdata, "")
        width  = get_value_from_tree(["width",  "_"],  objdata, 0)
        height = get_value_from_tree(["height", "_"],  objdata, 0)
        
        tilemap = TilemapLayer.new($tileset, width, height)
        tilemap.set_data(get_value_from_tree(["data"], objdata, []))
        tilemap.set_metadata(TilemapMetadata.new(:name  => name,
                                                 :z_pos => get_value_from_tree(["z-pos", "_"],  objdata, 0),
                                                 :x_offset => get_value_from_tree(["x-offset", "_"],  objdata, 0),
                                                 :y_offset => get_value_from_tree(["y-offset", "_"],  objdata, 0)
                                                 ))

        case name 
        when "background"
          @background = tilemap
        when "interactive"
          @interactive = tilemap
        when "interactivebackground"
          @interactivebackground = tilemap
        when "background"
          @background = tilemap
        else
          puts "Unknown Tilemap: #{name}"
        end
      else
        puts "Unknown object: '#{objtype}'"
        pos = get_value_from_tree(["pos"], objdata, [0,0])
        obj = ObjMapSpriteObject.new(make_sprite($datadir + "images/unknown.png"), 
                                     CL_Pointf.new(pos[0], pos[1]), make_metadata(nil))
        obj.to_object.set_metadata(make_metadata(UnknownGameObject.new(objtype, objdata, obj)))
      
        @objects.add_object(obj.to_object)
      end
    }
    
    @layers += [@background, @interactivebackground, @interactive, @foreground, @objects]

    @editormap = EditorMap.new(true)
    @layers.each {|layer| 
      if layer then 
        @editormap.add_layer(layer.to_layer()) 
      end 
    }
    
    # FIXME: Data might not get freed since its 'recursively' refcounted
    @editormap.set_data(self)

    @current_layer = 2

    puts @editormap
    puts @layers
  end

  def activate(workspace)
    $gui.workspace.set_map(@editormap)
    puts "Activate: #{@current_layer} #{@layers.inspect}"
    TilemapLayer.set_current(@layers[@current_layer])
    ObjectLayer.set_current(@objects)
    
    connect(@editormap.sig_change(), proc{
              puts "blabl"
              $gui.on_map_change()
            })
  end

  def save(filename)
    print "Save Sector to '", filename, "'\n"

    ## Insert your load code here
    f = File.new(filename, "w")
    f.write(";; Generated by Flexlay Editor\n")
    f.write("(windstille-sector\n")
    f.write("  (version 2)\n")
    f.write("  (ambient-color .666 .666 .666)")
    f.write("  (name   \"%s\")\n" % @name)
    f.write("  (music  \"%s\")\n" % @music)

    f.write("  (objects\n")
    save_tilemap = proc {|name, tilemap|
      if tilemap then
        width  = tilemap.get_width()
        height = tilemap.get_height()
        f.write("  (tilemap (name \"%s\") (width %d) (height %d) (z-pos %d)\n" % \
                [name, width, height, tilemap.get_metadata().z_pos]) # FIXME: add escaping to strings
        f.write("    (data")
        tilemap.get_data().each_with_index {|item, i|
          if (i % width == 0) then
            f.write("\n      ")
          end
          f.write("%d " % item)
        }
        f.write("\n     ))\n")
      end
    }

    save_tilemap.call("background",  @background)
    save_tilemap.call("interactivebackground", @interactivebackground)
    save_tilemap.call("interactive", @interactive)
    save_tilemap.call("foreground",  @foreground)

    for obj in @objects.get_objects()
      if obj.get_data() then
        obj.get_data().save(f)
      end
    end

    f.write("   )\n")

    f.write(" )\n\n")
    f.write(";; EOF ;;\n")
    f.close()
  end
end

# EOF #
