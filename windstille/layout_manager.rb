##  $Id$
##   ______ __               ___
##  |   ___|  |.-----.--.--.|   | .---.-.--.--.
##  |   ___|  ||  -__|_   _||   |_|  _  |  |  |
##  |__|   |__||_____|__.__||_____|___._|___  |
##                                      |_____|
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
##  Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
##  02111-1307, USA.


# Helper class that holds all necesarry paramter to handle layouting,
# could also extend Component instead
class LayoutComponent
  attr_reader :component, :name, :size,:expand, :fill, :padding

  # use nil for width and height if it should be determined
  # automatically
  def initialize(component, params)
    @component = component

    @name      = params[:name]
    @size      = params[:size]
    @expand    = params[:expand]
    @fill      = params[:fill]
    @padding   = params[:padding]
  end

  def get(name)
    return nil
  end
  
  def set_pos(x, y)
    @component.set_position(x, y)
  end
  
  def set_size(width, height)
    @component.set_size(width, height)
  end

  # Rearanges the layout to fit the current size
  def layout()
  end

  def LayoutComponent.create_from_sexpr(rect, sexpr, parent)
    case sexpr.car()
    when :vbox
    when :hbox
    when :panel
      
    else
      create(sexpr.car().value(), rect, sexpr.cdr(), parent)
    end
  end
  
  def LayoutComponent.create(type, rect, sexpr, parent)
    case type
    when :vbox
      box = LayoutBox.new(type, rect, sexpr, parent)
      return box

    when :hbox
      box = LayoutBox.new(type, rect, sexpr, parent)
      return box
      
    else
      return LayoutComponent.new(create_raw(type, rect, sexpr, parent), 
                                 :name    => sexpr.get_value([:name,    '_'], nil),
                                 :size    => sexpr.get_value([:size,    '_'], nil),
                                 :expand  => sexpr.get_value([:expand,  '_'], true),
                                 :fill    => sexpr.get_value([:fill,    '_'],    true),
                                 :padding => sexpr.get_value([:padding, '_'], 0))
    end
  end

  def LayoutComponent.create_raw(type, rect, sexpr, parent)
    case type
    when :panel     

    when :editormap
      return EditorMapComponent.new(rect, parent)      
      
    when :menubar
      return CL_Menu.new_from_spec(sexpr.get_value(['spec', '_'], []),
                                   parent)

    when :button
      return CL_Button.new(rect,
                           sexpr.get_value(['label', '_'], []),
                           parent)

    when :label
      return CL_Label.new(CL_Point.new(rect.top, rect.left),
                          sexpr.get_value(['label', '_'], []),
                          parent)

    when :listbox
      return CL_ListBox.new(rect, parent)
      
    when :inputbox
      return CL_InputBox.new(rect, parent)

    when :radiobutton
      return CL_RadioButton.new(CL_Point.new(rect.left, rect.top),
                                sexpr.get_value(['label', '_'], []),
                                parent)

    when :radiogroup
      return CL_RadioGroup.new()

    when :checkbox
      return CL_CheckBox.new(CL_Point.new(rect.left, rect.top),
                             sexpr.get_value(['label', '_'], []),
                             parent)

    when :buttonpanel
      return ButtonPanel.new_from_spec(rect.left, rect.top, rect.get_width(), rect.get_height(), true,
                                       sexpr.get_value([:spec, '_'], []), parent)

    when :tileselector
      return TileSelector.new(rect, parent)
      
    when :objectselector
      return ObjectSelector.new(rect, 
                                sexpr.get_value([:objectwidth, '_'], 42), 
                                sexpr.get_value([:objectheight, '_'], 42),
                                parent)

    when :minimap
      @minimap = Minimap.new(nil, rect, parent)
      
    else
      raise "Unknonwn Component type '#{type.inspect}'"

    end
  end
end

class LayoutBox < LayoutComponent
  def initialize(type, rect, sexpr, parent)
    super(nil,
          :name    => sexpr.get_value([:name,    '_'], nil),
          :size    => sexpr.get_value([:size,    '_'], nil),
          :expand  => sexpr.get_value([:expand,  '_'], true),
          :fill    => sexpr.get_value([:fill,    '_'],    true),
          :padding => sexpr.get_value([:padding, '_'], 0))

    @type       = type # :vbox or :hbox
    @x          = rect.left
    @y          = rect.top
    @width      = rect.get_width()
    @height     = rect.get_height()
    @parent     = parent
    @components = []
    @homogenus  = false

    sexpr.get(:components, SExpression.new()).each_pair() { |name, value|
      @components.push(LayoutComponent.create(name, CL_Rect.new(0, 0, 256, 256), value, @parent))
    }

    layout()
  end

  def get(name)
    @components.each() { |i|
      if i.name == name then
        return i
      elsif i.is_a?(LayoutBox) then
        a = i.get(name)
        if a then return a end
      end
    }
    return nil
  end

  def add(type, spec)
    @components.push([type, spec, nil])
  end
  
  def set_pos(x, y)
    @x = x
    @y = y
  end

  def set_size(width, height)
    @width  = width
    @height = height
    layout()
  end

  def layout()
    x = @x
    y = @y

    len = 0
    num = 0
    @components.each() { |component|
      if component.size then
        len += component.size
      else
        num += 1
      end
    }

    if @type == :vbox     
      avlen = (@height - len) / num
      
      @components.each() { |component|
        component.set_pos(x + component.padding, y + component.padding)

        if component.size then
          component.set_size(@width - component.padding*2, component.size - component.padding*2)
          y += component.size
        else
          component.set_size(@width - component.padding*2, avlen - component.padding*2)
          y += avlen
        end
      }
    elsif @type == :hbox
      avlen = (@width - len) / num
      
      @components.each() { |component|
        component.set_pos(x + component.padding, y + component.padding)

        if component.size then
          component.set_size(component.size - component.padding*2, @height - component.padding*2)
          x += component.size
        else
          component.set_size(avlen - component.padding*2, @height - component.padding*2)
          x += avlen
        end
      }
    else
      raise "LayoutBox: Unknown type #{type}"
    end
  end
end

## EOF ##
