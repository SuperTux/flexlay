require "rexml/document"
require "worldobjs.rb"

class WorldMap
  def initialize()
    @name        = ""
    @description = ""
    @author      = ""
    @music       = "pingus-1.it"

    @objects   = ObjectLayer.new()
    @editormap = EditorMap.new()
    @editormap.add_layer(@objects.to_layer())
    @editormap.set_metadata(self)
    @editormap.set_bounding_rect(CL_Rect.new(0, 0, 2400, 600));

    load($datadir + 'worldmaps/tutorial.xml')
  end    

  def activate(workspace)
    workspace.set_map(@editormap)
    ObjectLayer.set_current(@objects)
    connect(@editormap.sig_change(), proc{$gui.on_map_change()})
  end

  def load(filename)
    doc = REXML::Document.new(File.new(filename))

    # @version = doc.elements['/pingus-worldmap/version'].text.to_i
    reader = XMLReader.new(doc.elements['/pingus-worldmap/head'])
    @name        = reader.read("name",   :string,  "Some Worldmap")
    @description = reader.read("name",   :string,  "")
    @music       = reader.read("name",   :string,  "pingus-1.it")
    @width       = reader.read("width",  :int, 800)
    @height      = reader.read("height", :int, 600)

    @editormap.set_bounding_rect(CL_Rect.new(0, 0, @width, @height));

    doc.elements['/pingus-worldmap/objects'].elements.each {|object|
      case object.name
      when "surface"
        reader = XMLReader.new(object)
        name  = reader.read("name",     :string, "foo")
        image = reader.read("surface",  :image,  "")
        pos   = reader.read("position", :vector, CL_Pointf.new(0,0))

        obj = ObjMapSpriteObject.new(CL_Sprite.new(image, $resources), 
                                     pos,
                                     make_metadata(nil))
        
        @objects.add_object(obj.to_object())
      else
        puts "Unknown worldmap object: '#{object.name}'"
      end
    }
    
    doc.elements['/pingus-worldmap/graph/nodes'].elements.each {|object|
      case object.name 
      when "leveldot"
        reader = XMLReader.new(object)
        pos  = reader.read("dot/position", :vector, CL_Pointf.new(0, 0))

        obj = ObjMapSpriteObject.new(CL_Sprite.new("core/worldmap/dot_green", $resources), 
                                     pos,
                                     make_metadata(nil))
        
        @objects.add_object(obj.to_object())      
      else
        puts "Unknown graph object '#{object.name}'"
      end
    }

    doc.elements['/pingus-worldmap/graph/edges'].elements.each {|edge|
      
    }
  end
end

# EOF #
