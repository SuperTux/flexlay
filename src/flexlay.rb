class EditorMap
  alias orig_get_metadata get_metadata

  def get_metadata()
    return get_ruby_object(orig_get_metadata())
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

class SimpleFileDialog
  """Very simple FileDialog, mainly a placeholder until the real thing gets ready."""
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
    connect(@ok_button.sig_clicked(), @on_ok)
    connect(@cancel_button.sig_clicked(), @on_cancel)
    @callback = func
    @window.show()
  end
  
  def on_ok()
    @window.hide();
    @callback.call(@inputbox.get_text())
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
    @cancel = CL_Button(CL_Rect.new(CL_Point.new(230, 35), CL_Size.new(50, 25)), "Cancel",
                        @window.get_client_area())
    connect(@cancel.sig_clicked(), @on_cancel)
    connect(@ok.sig_clicked(), @on_ok)
  end

  def on_cancel()
    @window.hide()
  end
  
  def on_ok()
    @window.hide()
    if @callback != nil
      @callback.call(*get_values())
    end
  end

  def set_callback(c)
    @callback = c
  end
  
  def get_values()
    def get_value(item)
      (type, label, comp) = item
      if type == "int":
               return int(comp.get_text())
      elsif type == "string":
                  return comp.get_text()
      else
        return nil
      end
    end
    
    return map(get_value, @items)
  end
  
  def add_int(name, value = 0)
    @items.append(["int",
                   CL_Label.new(CL_Point.new(10, 10), name,
                                @window.get_client_area()),
                   CL_InputBox.new(CL_Rect.new(CL_Point.new(110, 10), CL_Size.new(200, 25)),
                                   @window.get_client_area())])
    @items[-1][2].set_text(str(value))
    update()
  end
  
  def add_string(name, value = "")
    @items.append(["string",
                   CL_Label.new(CL_Point.new(10, 10), name,
                                @window.get_client_area()),
                   CL_InputBox.new(CL_Rect.new(CL_Point.new(110, 10), CL_Size.new(200, 25)),
                               @window.get_client_area())])
    @items[-1][2].set_text(value)
    update()    
  end

  def update()
    y = 10
    for (type, label, comp) in @items
      if type == "int" or type == "string"
        label.set_position(10, y)
        comp.set_position(110, y)
        y += 25

        @cancel.set_position(200, y)
        @ok.set_position(260, y)
        @window.set_size(330, y + 60)
      end
    end
  end
end

# EOF #
