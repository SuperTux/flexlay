# Flexlay - A Generic 2D Game Editor
# Copyright (C) 2004 Ingo Ruhnke <grumbel@gmail.com>
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.

require "sexpr.rb"

# Helper class that holds all necesarry paramter to handle layouting,
# could also extend Component instead
class LayoutComponent
  attr_reader :component, :name, :size,:expand, :fill, :padding

  # use nil for width and height if it should be determined
  # automatically
  def initialize(component, child, params)
    @component = component
    @child     = child

    @name      = params[:name]
    @size      = params[:size]
    @expand    = params[:expand]
    @fill      = params[:fill]
    @padding   = params[:padding]
  end

  def get(name)
    if @child then
      return @child.get(name)
    else
      return nil
    end
  end
  
  def set_pos(x, y)
    if @component then
      @component.set_position(x, y)
    end
  end
  
  def set_size(width, height)
    if @component then
      @component.set_size(width, height)
    end

    if @child then
      @child.set_size(width, height)
    end
  end

  # Rearanges the layout to fit the current size
  def layout()
    if @child then
      return @child.layout()
    end
  end

  def LayoutComponent.create_from_sexpr(rect, sexpr, parent)
    create(sexpr.car().value(), rect, sexpr.cdr(), parent)
  end
  
  def LayoutComponent.create(type, rect, sexpr, parent)
    # puts "Create: #{type}"
    case type
    when :vbox
      return LayoutBox.new(type, rect, sexpr, parent)

    when :hbox
      return  LayoutBox.new(type, rect, sexpr, parent)

    when :panel
      panel = Panel.new(rect, parent)
      return LayoutComponent.new(panel, 
                                 LayoutBox.new(sexpr.get_value([:layout, '_'], :vbox),
                                               Rect.new(0, 0, rect.get_width(), rect.get_height()), 
                                               sexpr, panel),
                                 :name    => sexpr.get_value([:name,    '_'], nil),
                                 :size    => sexpr.get_value([:size,    '_'], nil),
                                 :expand  => sexpr.get_value([:expand,  '_'], true),
                                 :fill    => sexpr.get_value([:fill,    '_'],    true),
                                 :padding => sexpr.get_value([:padding, '_'], 0))

    when :tab
      return TabComponent.new(rect, sexpr, parent)

    else
      return LayoutComponent.new(create_raw(type, rect, sexpr, parent), 
                                 nil,
                                 :name    => sexpr.get_value([:name,    '_'], nil),
                                 :size    => sexpr.get_value([:size,    '_'], nil),
                                 :expand  => sexpr.get_value([:expand,  '_'], true),
                                 :fill    => sexpr.get_value([:fill,    '_'],    true),
                                 :padding => sexpr.get_value([:padding, '_'], 0))
    end
  end

  def LayoutComponent.create_raw(type, rect, sexpr, parent)
    case type

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
      return CL_Label.new(Point.new(rect.top, rect.left),
                          sexpr.get_value(['label', '_'], []),
                          parent)

    when :listbox
      return CL_ListBox.new(rect.to_cl(), parent)
      
    when :inputbox
      return CL_InputBox.new(rect.to_cl(), parent)

    when :radiobutton
      return CL_RadioButton.new(Point.new(rect.left, rect.top).to_cl(),
                                sexpr.get_value(['label', '_'], []),
                                parent)

    when :radiogroup
      return CL_RadioGroup.new()

    when :checkbox
      return CL_CheckBox.new(Point.new(rect.left, rect.top).to_cl(),
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

class TabComponent < LayoutComponent
  def initialize(rect, sexpr, parent)
    super(nil, nil,
          :name    => sexpr.get_value([:name,    '_'], nil),
          :size    => sexpr.get_value([:size,    '_'], nil),
          :expand  => sexpr.get_value([:expand,  '_'], true),
          :fill    => sexpr.get_value([:fill,    '_'],    true),
          :padding => sexpr.get_value([:padding, '_'], 0))
    
    @childs = []

    sexpr.get(:components, SExpression.new()).each_pair() { |name, value|
      @childs.push(LayoutComponent.create(name, Rect.new(0, 0, 256, 256), value, parent))
    }
  end

  def get(name)
    @childs.each() { |i| 
      if i.name == name then
        return i
      end
    }
    return nil
  end
  
  def set_pos(x, y)
    @childs.each() { |i| i.set_pos(x, y) }
  end
  
  def set_size(width, height)
    @childs.each() { |i| i.set_size(width, height) }
  end

  # Rearanges the layout to fit the current size
  def layout()
    @childs.each() { |i| i.layout() }
  end
end

class LayoutBox < LayoutComponent
  def initialize(type, rect, sexpr, parent)
    super(nil, nil,
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
      @components.push(LayoutComponent.create(name, Rect.new(0, 0, 256, 256), value, @parent))
    }

    layout()
  end

  def get(name)
    @components.each() { |i|
      if i.name == name then
        return i
      else
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
    layout()
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
