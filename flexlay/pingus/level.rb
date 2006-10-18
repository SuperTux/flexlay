require "rexml/document"
require "worldobjs.rb"

class Level
  attr_reader :objects, :editormap
  attr_accessor :levelname, :description, :author, :number_of_pingus, :number_to_save, :ambient_light, :comment, :levelsize, :actions

  def initialize(filename = nil)
    @objects   = ObjectLayer.new()
    @editormap = EditorMap.new()
    @editormap.add_layer(@objects.to_layer())
    @editormap.set_metadata(self)
    @editormap.set_bounding_rect(CL_Rect.new(0, 0, 2400, 600));

    if filename then
      load(filename)
    end  
  end

  def load(filename)
    doc = REXML::Document.new(File.new(filename))

    @version = doc.elements['/pingus-level/version'].text.to_i

    # Parse the header
    header = doc.elements['/pingus-level/head'].elements
    @levelname   = header['levelname'].text
    @description = header['description'].text
    @author      = header['author'].text
    @number_of_pingus = header['number-of-pingus'].text.to_i
    @number_to_save   = header['number-to-save'].text.to_i

    if header['ambient-light/red'] then
      @ambient_light = CL_Color.new((header['ambient-light/red'].text.to_f * 255).to_i,
                                  (header['ambient-light/green'].text.to_f * 255).to_i,
                                  (header['ambient-light/blue'].text.to_f  * 255).to_i,
                                  (header['ambient-light/alpha'].text.to_f * 255).to_i)
    else
      @ambient_light = CL_Color.new(1, 1, 1, 1);
    end

    @comment = header['comment'].text
    @levelsize = CL_Size.new(header['levelsize/width'].text.to_i,
                             header['levelsize/height'].text.to_i)
    @editormap.set_bounding_rect(CL_Rect.new(CL_Point.new(0, 0), @levelsize))

    @actions = header['actions'].elements.inject({}) {|h, e| h[e.name] = e.text.to_i; h }
    
    doc.elements.each("pingus-level/objects/*") { |element|
      worldobj = WorldObj.new(element.name, element)

      obj = ObjMapSpriteObject.new(CL_Sprite.new(worldobj.get_image(), $resources), 
                                   worldobj.get_pos(), 
                                   make_metadata(worldobj))
      worldobj.set_data(obj)
      
      @objects.add_object(obj.to_object())
    }
  end

  def save(filename)
    doc = REXML::Document.new()
    doc.xml_decl.xmldecl("1.0", "ISO-8859-1", false)
    root = doc.add_element("pingus-level")
    head = root.add_element("head")
    head.add_element("levelname").add_text(@levelname)
    head.add_element("description").add_text(@description)
    head.add_element("author").add_text(@author)
    head.add_element("number_of_pingus").add_text(@number_of_pingus.to_s)
    head.add_element("number_to_save").add_text(@number_to_save.to_s)
    head.add_element("comment").add_text(@comment)
    # FIXME: More to come
    objects = root.add_element("objects")
    @objects.get_objects().each { |object|
      worldobj = object.get_metadata()
      if worldobj then
        worldobj.write_xml(objects)
      end
    }

    f = File.new(filename, "w")
    doc.write(f, 0)
    f.close()
  end

  def activate(workspace)
    workspace.set_map(@editormap)
    ObjectLayer.set_current(@objects)
    connect(@editormap.sig_change(), proc{$gui.on_map_change()})
  end
end

# EOF #
