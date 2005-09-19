##  $Id$
## 
##  Flexlay - A Generic 2D Game Editor
##  Copyright (C) 2002 Ingo Ruhnke <grumbel@gmx.de>
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
##  Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.

# A parser for command line arguments
class CommandLineOption
  attr_reader :short, :long, :description

  def initialize(short, long, description)
    @short = short
    @long  = long
    @description = description
  end
end

class CommandLine
  def initialize(&block)
    @options = []
    @name    = nil
    @usage   = nil
    @help    = []
    @description = nil

    if block then
      instance_eval(&block);
    end
  end

  def name(name)
    @name = name
  end

  def usage(usage)
    @usage = usage
  end

  def description(description)
    @description = description
  end

  def group(group)
    @help.push("\n" + group + ":")
  end

  def text(text)
    @help.push(text)
  end

  def option(short, long, argument, description)
    cmd = CommandLineOption.new(short, long, description)
    @options.push(cmd)
    @help.push(cmd)
  end

  def parse(args)
    args = args.clone()
    print_help()

    yield()
  end

  def print_help()
    puts(@name) if @name
    puts("Usage: " + @usage) if @usage
    puts("")
    puts(@description) if @description

    @help.each() { |item|
      if item.is_a?(String)
        puts(item)
      elsif item.is_a?(CommandLineOption)
        puts("   -#{item.short}, --#{item.long}   #{item.description}")
      end
    }
  end
end

cmd = CommandLine.new() {
  name("Windstille Editor V0.1")
  usage("windstille-editor [OPTION]... [FILE]...")
  description("Editor for editing Windstille map files.")
  
  group("Display")
  option("f", "fullscreen", nil,            "Launch in fullscreen mode")
  option("g", "geometry",   "WIDTHxHEIGHT", "Launch in the given resolution")

  text("If you have throuble launching, try to cleanup ~/.windstille-editor/config.scm or contact grumbel@gmx.de")
}

cmd.parse(ARGV) { |option, argument|
  case option
  when "f"
    
  when "g"
    
  when :rest
    
  end
}

# EOF #
