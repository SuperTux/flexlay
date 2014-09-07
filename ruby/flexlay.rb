class TilemapLayer
  alias orig_get_metadata get_metadata
  alias orig_set_metadata set_metadata

  def set_metadata(data)
    orig_set_metadata(make_metadata(data))
  end

  def get_metadata()
    return get_ruby_object(orig_get_metadata())
  end

  def each(x, y, width, height)
    data = get_data()
    (y..height-1).each{
      (x..width-1).each{
        yield(data[y*get_width() + x])
      }
    }
  end
end

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
  def get_data()
    return get_ruby_object(get_metadata())
  end

  def set_data(data)
    set_metadata(make_metadata(data))
  end
end

class Menubar
  def Menubar.new_from_spec(menubarspec, parent)
    menu = Menubar.new(parent)

    menubarspec.each { |(title, *menu_spec)|
      menu_spec.each{ |(name, callback)|
        menu.add_item("#{title}/#{name}", callback)
      }
    }

    return menu
  end
end

class ButtonPanel
  def ButtonPanel.new_from_spec(x, y, width, height, horizontal, spec, parent)
    buttonpanel = ButtonPanel.new(Rect.new(x, y, width, height), horizontal, parent)

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
end

puts PropertyValue::TYPE_BOOL

class GenericDialog
  def set_block()
    callback = proc{ |*args| yield(*args) }
    set_callback(callback)
  end

  def set_callback(callback)
    set_ok_callback(proc{
                      values = get_values()
                      ruby_values = values.map{ |v|
                        case v.get_type()
                        when PropertyValue::TYPE_BOOL
                          v.get_bool()
                        when PropertyValue::TYPE_INT
                          v.get_int()
                        when PropertyValue::TYPE_FLOAT
                          v.get_float()
                        when PropertyValue::TYPE_STRING
                          v.get_string()
                        end
                      }
                      callback.call(*ruby_values)
                    })
  end
end

# EOF #
