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


class Icon
  def set_callback(func)
    connect(sig_clicked(), func)
  end
end

class Menu
  alias_method :orig_add_item, :add_item

  def add_item(*params)
    if params.length == 2 then
      (text, func) = params
      i = orig_add_item(text)
    else
      (sprite, text, func) = params
      i = orig_add_item(sprite, text)
    end

    if func != nil
      connect(sig_clicked(i), func)
    end
  end
end

class CL_Menu
  def add_item(name, func)
    item = create_item(name)
    connect_cl(item.sig_clicked(), func)
  end

  def CL_Menu.new_from_spec(menubarspec, parent)
    menu = CL_Menu.new(parent)

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

class GenericDialog
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
end

# EOF #
