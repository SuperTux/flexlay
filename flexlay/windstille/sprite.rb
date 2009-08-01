##  $Id$
##   ______ __               ___
##  |   ___|  |.-----.--.--.|   | .---.-.--.--.
##  |   ___|  ||  -__|_   _||   |_|  _  |  |  |
##  |__|   |__||_____|__.__||_____|___._|___  |
##                                      |_____|
##  Copyright (C) 2004 Ingo Ruhnke <grumbel@gmx.de>
##
##  This program is free software: you can redistribute it and/or modify
##  it under the terms of the GNU General Public License as published by
##  the Free Software Foundation, either version 3 of the License, or
##  (at your option) any later version.
##  
##  This program is distributed in the hope that it will be useful,
##  but WITHOUT ANY WARRANTY; without even the implied warranty of
##  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
##  GNU General Public License for more details.
##  
##  You should have received a copy of the GNU General Public License
##  along with this program.  If not, see <http://www.gnu.org/licenses/>.

require "sexpr.rb"

class SpriteAction
  attr_reader :name, :speed, :offset, :images

  def initialize(sexpr)
    value.each_pair() {|name, value|
      case name
      when :name
        @offset = value.car().value()

      when :speed 
        @speed = value.car().value()

      when :images
        @images = value.car().value()

      when :'image-grid'
        # implement me

      when :offset
        @offset = value.value()

      else
        puts "Sprite: Unknown tag #{name}"
      end
    end
  end
end

class Sprite
  attr_reader :actions

  def initialize(filename)
    @actions = []

    sexpr = SExpression.new_from_file(filename)
    
    if sexpr.car().value() == :sprite then
      sexpr.cdr().each_pair() { |name, value|
        case name
        when :action
          @actions.push(SpriteAction.new(value))
        else
          puts "Sprite: Unknown tag #{name}"
        end
      }
    else
      raise "Sprite: #{filename} not a sprite file"
    end
  end
  
end

## EOF ##
