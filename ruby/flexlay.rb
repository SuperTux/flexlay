# Ruby 3.x + modern SWIG: bare `alias foo bar` on C-extension methods
# raises NameError ("undefined method ... for class").  Use prepend so
# the original SWIG method remains reachable via super().

# Resolve a SWIG %inline factory.  After `include Flexlay_wrap` the
# functions are private instance methods of Object; `defined?` does not
# see them.  Prefer the module singleton, then a private respond_to?
def flexlay_factory(name)
  if defined?(Flexlay_wrap) && Flexlay_wrap.respond_to?(name)
    return Flexlay_wrap.method(name)
  end
  if respond_to?(name, true)
    return method(name)
  end
  nil
end

# Fallback for statics SWIG may omit (set_current / current).
# Ruby 3.4 rejects `def Foo::Bar.baz`; use define_singleton_method.
if defined?(Flexlay_wrap::TilemapLayer) && !Flexlay_wrap::TilemapLayer.respond_to?(:set_current)
  Flexlay_wrap::TilemapLayer.define_singleton_method(:set_current) do |t|
    if (f = flexlay_factory(:TilemapLayer_set_current))
      f.call(t)
    else
      raise NoMethodError, "TilemapLayer.set_current not wrapped by SWIG"
    end
  end
end
if defined?(Flexlay_wrap::ObjectLayer) && !Flexlay_wrap::ObjectLayer.respond_to?(:set_current)
  Flexlay_wrap::ObjectLayer.define_singleton_method(:set_current) do |c|
    if (f = flexlay_factory(:ObjectLayer_set_current))
      f.call(c)
    else
      raise NoMethodError, "ObjectLayer.set_current not wrapped by SWIG"
    end
  end
end

module TilemapLayerMetadata
  def set_metadata(data)
    super(make_metadata(data))
  end

  def get_metadata
    get_ruby_object(super())
  end
end
TilemapLayer.prepend(TilemapLayerMetadata)

class TilemapLayer
  def each(x, y, width, height)
    data = get_data()
    (y...height).each do |iy|
      (x...width).each do |ix|
        yield(data[iy * get_width() + ix])
      end
    end
  end
end

module EditorMapMetadata
  def set_data(data)
    set_metadata(data)
  end

  def get_data
    get_metadata
  end

  def set_metadata(data)
    super(make_metadata(data))
  end

  def get_metadata
    get_ruby_object(super())
  end
end
EditorMap.prepend(EditorMapMetadata)

module ObjMapObjectMetadata
  def get_data
    get_ruby_object(get_metadata())
  end

  def set_data(data)
    set_metadata(make_metadata(data))
  end
end
ObjMapObject.prepend(ObjMapObjectMetadata)

# Open the SWIG class explicitly — after include Flexlay_wrap, a bare
# `class Icon` can sometimes bind to a different constant.
class Flexlay_wrap::Icon
  def set_callback(func)
    connect(sig_clicked(), func)
  end
end
# Alias for code that still refers to Icon without the module prefix.
Icon = Flexlay_wrap::Icon unless defined?(Icon) && Icon.equal?(Flexlay_wrap::Icon)

module MenuAddItem
  def add_item(*params)
    if params.length == 2
      text, func = params
      i = super(text)
    else
      sprite, text, func = params
      i = super(sprite, text)
    end

    connect(sig_clicked(i), func) unless func.nil?
    i
  end
end
Menu.prepend(MenuAddItem)

class Flexlay_wrap::CL_Menu
  def add_item(name, func)
    item = create_item(name)
    connect(item.sig_clicked(), func)
  end

  # Factory used by the layout loader.
  def self.new_from_spec(menubarspec, parent)
    # Prefer the 1-arg factory; Method#arity is unreliable for SWIG C funcs.
    if (factory = flexlay_factory(:CL_Menu_create))
      menu = factory.call(parent)
    elsif (factory = flexlay_factory(:CL_Menu_create_rect))
      menu = factory.call(CL_Rect.new(CL_Point.new(0, 0), CL_Size.new(100, 20)), parent)
    else
      begin
        menu = CL_Menu.new(parent, nil, false)
      rescue ArgumentError
        begin
          menu = CL_Menu.new(parent)
        rescue ArgumentError
          menu = CL_Menu.new(CL_Rect.new(CL_Point.new(0, 0), CL_Size.new(100, 20)), parent)
        end
      end
    end

    menubarspec.each do |(title, *menu_spec)|
      menu_spec.each do |(name, callback)|
        menu.add_item("#{title}/#{name}", callback)
      end
    end

    menu
  end
end
CL_Menu = Flexlay_wrap::CL_Menu unless defined?(CL_Menu) && CL_Menu.equal?(Flexlay_wrap::CL_Menu)

class ButtonPanel
  attr_reader :panel, :items

  def initialize(x, y, width, height, horizontal, parent, &block)
    rect = CL_Rect.new(CL_Point.new(x, y), CL_Size.new(width, height))
    factory = flexlay_factory(:Panel_create)
    @panel = factory ? factory.call(rect, parent) : Panel.new(rect, parent)
    @pos   = 2
    @horizontal = horizontal
    @items = {}

    if block then
      instance_eval(&block)
    end
  end

  def set_position(x, y)
    @panel.set_position(x, y)
  end

  def set_size(w, h)
    @panel.set_size(w, h)
  end

  def ButtonPanel.new_from_spec(x, y, width, height, horizontal, spec, parent)
    buttonpanel = ButtonPanel.new(x, y, width, height, horizontal, parent)

    spec.each{ |(type, *data)|
      case type
      when :icon
        buttonpanel.items[data[0]] = buttonpanel.add_icon(data[1], data[2])
      when :toggle
        buttonpanel.items[data[0]] = buttonpanel.add_icon(data[1], data[2])
      when :small_icon
        buttonpanel.items[data[0]] = buttonpanel.add_small_icon(data[1], data[2])
      when :seperator
        buttonpanel.add_separator()
      else
        raise "ButtonPanel: Unknown type #{type}"
      end
    }
    
    return buttonpanel
  end

  def make_icon(rect, image, tooltip)
    sprite = make_sprite(image)
    factory = flexlay_factory(:Icon_create)
    if factory
      factory.call(rect, sprite, tooltip, @panel)
    else
      Icon.new(rect, sprite, tooltip, @panel)
    end
  end

  def add_small_icon(image = nil, callback = nil, tooltip = "")
    if (@horizontal)
      icon = make_icon(CL_Rect.new(CL_Point.new(@pos,  2), CL_Size.new(16, 32)),
                       image, tooltip)
    else
      icon = make_icon(CL_Rect.new(CL_Point.new(2, @pos), CL_Size.new(16, 32)),
                       image, tooltip)
    end
    
    @pos += 16
    if (callback)
      icon.set_callback(callback)
    end
    return icon
  end

  def add_icon(image = nil, callback = nil, &block)
    tooltip = ""
    if block then
      callback = block
    end

    if (@horizontal)
      icon = make_icon(CL_Rect.new(CL_Point.new(@pos,  2), CL_Size.new(32, 32)),
                       image, tooltip)
    else
      icon = make_icon(CL_Rect.new(CL_Point.new(2, @pos), CL_Size.new(32, 32)),
                       image, tooltip)
    end
    
    @pos += 32
    if (callback)
      icon.set_callback(callback)
    end
    return icon
  end
  
  def add_separator()
    @pos += 16
  end
  
  def show(b)
    @panel.show(b)
  end
end

# Very simple FileDialog, mainly a placeholder until the real thing gets ready.
class SimpleFileDialog
  @window   = nil
  @inputbox = nil
  @ok_button     = nil
  @cancel_button = nil
  @callback = nil
  
  def initialize(title, ok, cancel, g)
    @window   = Window.new(CL_Rect.new(CL_Point.new(120, 200), CL_Size.new(560, 100)), title, g)
    @inputbox = CL_InputBox.new(CL_Rect.new(CL_Point.new(10, 10), CL_Size.new(530, 25)),
                            @window.get_client_area())
    @ok_button     = CL_Button.new(CL_Rect.new(CL_Point.new(490, 35), CL_Size.new(50, 25)), ok,
                               @window.get_client_area())
    @cancel_button = CL_Button.new(CL_Rect.new(CL_Point.new(430, 35), CL_Size.new(50, 25)), cancel,
                               @window.get_client_area())
    @window.hide()
  end
  
  def set_filename(filename)
    @inputbox.set_text(filename)
  end
  
  def get_filename()
    return @inputbox.get_text()
  end
  
  def run(func)
    connect(@ok_button.sig_clicked(), method(:on_ok))
    connect(@inputbox.sig_return_pressed(), method(:on_ok))
    connect(@cancel_button.sig_clicked(), method(:on_cancel))
    @callback = func
    @inputbox.set_focus()
    @window.show()
  end
  
  def on_ok()
    @window.hide();
    if @callback
      @callback.call(@inputbox.get_text())
    end
  end
  
  def on_cancel()
    @window.hide();
  end
end

class GenericDialog
  window = nil
  items  = nil
  ok     = nil
  cancel = nil
  callback = nil
  
  def initialize(title, gui)
    @items = []
    @window = Window.new(CL_Rect.new(CL_Point.new(100, 100), CL_Size.new(400, 100)), title, gui)
    @ok = CL_Button.new(CL_Rect.new(CL_Point.new(290, 35), CL_Size.new(50, 25)), "Ok",
                    @window.get_client_area())
    @cancel = CL_Button.new(CL_Rect.new(CL_Point.new(230, 35), CL_Size.new(50, 25)), "Cancel",
                            @window.get_client_area())
    connect(@cancel.sig_clicked(), method(:on_cancel))
    connect(@ok.sig_clicked(), method(:on_ok))
  end

  def on_cancel()
    @window.hide()
  end
  
  def on_ok()
    @window.hide()
    if @callback
      vals = []
      @items.each{|item|
        (type, label, comp) = item
        if type == "int"
          vals.push(comp.get_text().to_i)
        elsif type == "float"
          vals.push(comp.get_text().to_f)
        elsif type == "string"
          vals.push(comp.get_text())
        elsif type == "bool"
          vals.push(comp.is_checked())
        elsif type == "enum"
          comp.get_buttons().each{|button|
            if (button.is_checked()) then
              vals.push(button.get_text())
              break;
            end
          }
        end
      }
      @callback.call(*vals)
    end
  end

  def set_block()
    @callback = proc{ |*args| yield(*args) }
  end

  def set_callback(c)
    @callback = c
  end

  def add_label(text)
    @items.push(["void", 
                  CL_Label.new(CL_Point.new(10, 10), text, @window.get_client_area()),
                  nil])
    update()
  end

  def add_float(name, value = 0)
    @items.push(["float",
                 CL_Label.new(CL_Point.new(10, 10), name,
                              @window.get_client_area()),
                 CL_InputBox.new(CL_Rect.new(CL_Point.new(110, 10), CL_Size.new(200, 25)),
                                 @window.get_client_area())])
    @items[-1][2].set_text(value.to_s)
    update()
  end
    
  def add_bool(name, value = false)
    @items.push(["bool",
                  CL_Label.new(CL_Point.new(10, 10), name,
                               @window.get_client_area()),
                  CL_CheckBox.new(CL_Point.new(110, 10), 
                                  "",
                                  @window.get_client_area())])
    if value == true
      @items[-1][2].set_checked()
    end
    update()
  end

  def add_enum(name, types, value = "foo")
    group = CL_RadioGroup.new()
    types.each {|type| 
      radio = CL_RadioButton.new(CL_Point.new(0, 0),
                                 type, @window.get_client_area())
      radio.set_checked(type == value)
      group.add(radio)
    }
    @items.push(["enum",
                  CL_Label.new(CL_Point.new(10, 10), name,
                               @window.get_client_area()),
                 group])
    update()
  end

  def add_int(name, value = 0)
    @items.push(["int",
                 CL_Label.new(CL_Point.new(10, 10), name,
                              @window.get_client_area()),
                 CL_InputBox.new(CL_Rect.new(CL_Point.new(110, 10), CL_Size.new(200, 25)),
                                 @window.get_client_area())])
    @items[-1][2].set_text(value.to_s)
    update()
  end
  
  def add_string(name, value = "")
    @items.push(["string",
                 CL_Label.new(CL_Point.new(10, 10), name,
                              @window.get_client_area()),
                 CL_InputBox.new(CL_Rect.new(CL_Point.new(110, 10), CL_Size.new(200, 25)),
                                 @window.get_client_area())])
    @items[-1][2].set_text(value)
    update()    
  end

  def update()
    y = 10
    @items.each { |(type, label, comp)| 
      label.set_position(10, y)

      if type == "int" or type == "string" or type == "float" or type == "void" or type == "bool" then
        if comp then
          comp.set_position(110, y)
        end
        y += 25
      elsif type == "enum"
        y += 5
        comp.get_buttons.each {|radio|
          radio.set_position(110, y)
          y += 20
        }
        y += 5
      end
    }
  
    @cancel.set_position(200, y)
    @ok.set_position(260, y)
    @window.set_size(330, y + 60)
  end
end

# EOF #
