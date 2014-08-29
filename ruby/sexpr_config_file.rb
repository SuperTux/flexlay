##  $Id$
## 
##  Flexlay - A Generic 2D Game Editor
##  Copyright (C) 2002 Ingo Ruhnke <grumbel@gmx.de>
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

# Options is a class to manage configuration options and save/load of
# them
class SExprConfigFile
  # The name is used to derive the location of the config file in the
  # file system, name must not contain / or \
  def initialize(name, &block)
    @name    = name
    @defaults = {}
    @options = {}

    home = ENV['HOME']
    if home then
      dir = home + "/.#{name}"
      if not (File.exists?(dir) and File.directory?(dir)) then
        Dir::mkdir(dir)
      end
      @filename = home + "/.#{@name}/config.scm"
    else
      # assuming windows
      @filename = "#{name}-config.scm"
    end
    
    if block then
      instance_eval(&block);
      read()
    end
  end

  # Register default values
  def register(name, value)
    @defaults[name] = value
  end

  def get(name)
    if @options.has_key?(name) then
      return @options[name]
    elsif @defaults.has_key?(name) then
      return @defaults[name]
    else
      raise "Error: Options:get: don't have a #{name} option"
    end
  end

  def set(name, value)
    if @defaults.has_key?(name) then
      @options[name] = value
    else
      raise "Error: Options:set: don't have a #{name} option"
    end
  end

  def read()
    sexpr = SExpression.new_from_file(@filename)
    sexpr = sexpr.cdr()
    sexpr.each_pair() {|key, value|
      if @defaults.has_key?(key.to_s) then
        if @defaults[key.to_s].is_a?(Array) then
          @options[key.to_s] = value.value()
        else
          @options[key.to_s] = value.value()[0]
        end
      else
        puts "Warning: Ignoring unknown config key: #{key}"
      end
    }
  end

  def value2sexpr(value)
    if value.is_a?(Fixnum) or value.is_a?(Float) then
      return value.to_s
    elsif value.is_a?(String)
      return value.inspect
    elsif value.is_a?(TrueClass) or value.is_a?(FalseClass)
      if value then 
        return "#t"
      else
        return "#f"
      end
    elsif value.is_a?(Array)
      str = ""
      value.each() {|v| 
        str += value2sexpr(v)
        str += " "
      }
      return str
    else
      raise "Error: Unknown option type: #{value.class}"
    end
  end

  def write()
    # FIXME: Move this over to a SExpression writer
    f = File.new(@filename, "w")
    f.puts("(#{@name}-config")
    @defaults.each_pair {|key, value|
      if @options.has_key?(key) and @options[key] != nil then
        f.puts("    (%-20s %s)" % [key, value2sexpr(@options[key])])
      else
        f.puts(";;  (%-20s %s)" % [key, value2sexpr(value)])
      end
    }
    f.puts(")\n\n;; EOF ;;")
    f.close()
  end
end

# EOF #
