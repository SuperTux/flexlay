require "rexml/document"
require "xmlreader.rb"

$objects = {
  "entrance" => {
    "type"         => [:string, "generic"],
    "owner-id"     => [:int,     0],
    "direction"    => [:enum,   "misc", ["left", "right", "misc"]],
    "position"     => [:vector, CL_Point.new(0, 0)],
    "release-rate" => [:int,    25]},

  "exit" => {
    "position" => [:vector, CL_Point.new(0, 0)],
    "surface"  => [:image,  ""],
    "owner-id" => [:int,    0]},

  "fake-exit" => {
    "position" => [:vector, CL_Point.new(0, 0)]},

  "groundpiece" => {
    "position" => [:vector, CL_Point.new(0, 0)],
    "surface"  => [:image, ""]},

  "guillotine" => {
    "position" => [:vector, CL_Point.new(0, 0)]},
  
  "hammer" => {
    "position" => [:vector, CL_Point.new(0, 0)]},
  
  "hotspot" => {
    "position" => [:vector, CL_Point.new(0, 0)],
    "surface"  => [:image,  ""],
    "parallax" => [:float,  0]},
  
  "iceblock" => {
    "position" => [:vector, CL_Point.new(0, 0)]},
  
  "info-box" => {
    "position"  => [:vector, CL_Point.new(0, 0)],
    "info-text" => [:string, ""]},
  
  "laser-exit" => {
    "position" => [:vector, CL_Point.new(0, 0)]},
  
  "liquid" => {
    "position" => [:vector, CL_Point.new(0, 0)],
    "surface"  => [:image,  ""],
    "width"    => [:int,    1]},
  
  "rain-generator" => {},
  
  "smasher" => {
    "position" => [:vector, CL_Point.new(0, 0)]},
  
  "snow-generator" => {
    "intensity" => [:float, 1.0]},
  
  "solid-color-background" => {
    "color" => [:color, CL_Color.new(0, 0, 0, 255)]},
  
  "spike" => {
    "position" => [:vector, CL_Point.new(0, 0)]},
  
  "starfield-background" => {
    "small-stars"  => [:int, 100],
    "middle-stars" => [:int,  50],
    "large-stars"  => [:int,  25]},
  
  "surface-background" => {
    "surface"      => [:image, ""],
    "color"        => [:color, CL_Color.new(0, 0, 0, 0)],
    "para-x"       => [:float, 0.5],
    "para-y"       => [:float, 0.5],
    "scroll-x"     => [:float, 0],
    "scroll-y"     => [:float, 0],
    "stretch-x"    => [:float, 1],
    "stretch-y"    => [:float, 1],
    "keep-aspect"  => [:bool,  true]},

  "switch-door" => {
    "position" => [:vector, CL_Point.new(0, 0)]},
  
  "teleporter" => {
    "position" => [:vector, CL_Point.new(0, 0)],
    "target"   => [:vector, CL_Point.new(0, 0)]},

  "thunderstorm-background" => {
    "position" => [:vector, CL_Point.new(0, 0)]},
  
  "woodthing" => {
    "position" => [:vector, CL_Point.new(0, 0)]}}

class WorldObj
  def initialize(typename, rest)
    @data       = nil
    @typename   = typename # Type of the object ('groundpiece', 'hotspot', ...)
    @properties = {}       # Properties of the object ({'speed' => 10, 'paralax' => 10.0})
    
    if rest.class == REXML::Element then
      element = rest
      
      reader = XMLReader.new(element)
      $objects[typename].each {|k, v|
        @properties[k] = reader.read(k, v[0], v[1])
      }
    elsif rest.class == String then
      image = rest

      $objects[typename].each {|k, v|
        if k == "surface" then
          @properties[k] = [image]
        elsif k != "position" then
          @properties[k] = v[1]
        end
      }
    else
      raise "Error: Unknown type to WorldObj constructor"
    end
  end

  def write_xml(el)
    el = el.add_element(@typename)
    @properties.each{|k, v|
      el.add_element(k).add_text(v.to_s)
    }
  end

  def get_image()
    if @properties['surface'] then
      return @properties['surface'][0]
    else
      return "core/misc/404sprite"
    end
  end

  def get_pos()
    return @properties['position'] || CL_Pointf.new(0, 0)
  end

  def set_data(data)
    @data = data
    if @properties.has_key?('position') then
      @data.to_object().set_pos(get_pos())
      @properties.delete('position')   
    end
    set_modifier()
  end

  def set_modifier()
    if @properties['surface'] then
      modifier = @properties['surface'][1]
      case modifier
      when "ROT0"
        
      when "ROT90"
      when "ROT180"
        @data.flip_vertical()
        @data.flip_horizontal()
      when "ROT270"
      when "ROT0FLIP"
        @data.flip_horizontal()
      when "ROT90FLIP"
      when "ROT180FLIP"
        @data.flip_vertical()
      when "ROT270FLIP"
      else
        puts "Error: Unknown modifier: #{modifier}"
      end
    end
  end

  def property_dialog()
    puts "Property: #{@properties.inspect}"

    dialog = GenericDialog.new("'#{@typename}' Property Dialog", $gui.get_component())
    @properties.each{|k, v|
      case $objects[@typename][k][0]
      when :string
        dialog.add_string("#{k}: ", v)
      when :image
        dialog.add_string("#{k}: ", v)
      when :int
        dialog.add_int("#{k}: ", v)
      when :float
        dialog.add_float("#{k}: ", v)
      when :bool
        dialog.add_bool("#{k}: ", v)
      when :enum
        dialog.add_enum("#{k}: ", $objects[@typename][k][2], v)
      else
        puts "Warning: Ignoring '#{k}' property, type '#{$objects[@typename][k][0]}' is unknown"
      end
    }
    dialog.set_callback(proc{|message| 
                          @message = message
                        })
  end
end

# EOF #
