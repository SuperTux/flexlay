class EditorMap
  alias orig_get_metadata get_metadata
  alias orig_set_metadata set_metadata

  def set_data(data)
    orig_set_metadata(make_metadata(data))
  end

  def get_data()
    return get_ruby_object(orig_get_metadata())
  end

  def set_metadata(data)
    orig_set_metadata(make_metadata(data))
  end

  def get_metadata()
    return get_ruby_object(orig_get_metadata())
  end
end

class ObjMapObject
  alias orig_get_metadata get_metadata
  alias orig_set_metadata set_metadata

  def get_data()
    return get_ruby_object(orig_get_metadata())
  end

  def set_data(data)
    orig_set_metadata(make_metadata(data))
  end

  def get_metadata()
    return get_ruby_object(orig_get_metadata())
  end

  def set_metadata(data)
    orig_set_metadata(make_metadata(data))
  end
end


class Icon
  def set_callback(func)
    connect(sig_clicked(), func)
  end
end

class Menu
  alias_method :orig_add_item, :add_item

  def add_item(sprite, text, func)
    i = orig_add_item(sprite, text)
    if func != nil
            connect(sig_clicked(i), func)
    end
  end
end

class CL_Menu
  def add_item(name, func)
    item = create_item(name)
    connect(item.sig_clicked(), func)
  end
end

class ButtonPanel
  attr_reader :panel

  def initialize(x, y, width, height, horizontal, parent)
    @panel = Panel.new(CL_Rect.new(CL_Point.new(x, y), CL_Size.new(width, height)), parent)
    @pos   = 2
    @horizontal = horizontal
  end

  def add_small_icon(filename, callback = nil, tooltip = "")
    if (@horizontal)
      icon = Icon.new(CL_Rect.new(CL_Point.new(@pos,  2), CL_Size.new(16, 32)),
                      make_sprite(filename), tooltip, @panel);
    else
      icon = Icon.new(CL_Rect.new(CL_Point.new(2, @pos), CL_Size.new(16, 32)),
                      make_sprite(filename), tooltip, @panel);
    end
    
    @pos += 16
    if (callback)
      icon.set_callback(callback)
    end
    return icon
  end

  def add_icon(filename, callback = nil, tooltip = "")
    if (@horizontal)
      icon = Icon.new(CL_Rect.new(CL_Point.new(@pos,  2), CL_Size.new(32, 32)),
                      make_sprite(filename), tooltip, @panel);
    else
      icon = Icon.new(CL_Rect.new(CL_Point.new(2, @pos), CL_Size.new(32, 32)),
                      make_sprite(filename), tooltip, @panel);
    end
    
    @pos += 32
    if (callback)
      icon.set_callback(callback)
    end
    return icon
  end
  
  def add_seperator()
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
    # @items[-1][2].set_text(value.to_s)
    update()
  end

  def add_enum(name, types, value = "foo")
    group = CL_RadioGroup.new()
    types.each {|type| 
      group.add(CL_RadioButton.new(CL_Point.new(0, 0),
                         type, @window.get_client_area()))
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
