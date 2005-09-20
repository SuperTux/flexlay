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
  attr_reader :short, :long, :argument, :description

  def initialize(short, long, argument, description)
    @short = short
    @long  = long
    @argument = argument
    @description = description
  end
end

class CommandLineException < RuntimeError
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
    @help.push("\n" + text)
  end

  def option(short, long, argument, description)
    cmd = CommandLineOption.new(short, long, argument, description)
    @options.push(cmd)
    @help.push(cmd)
  end

  def parse(args)
    args = args.reverse()

    @stop_parsing = false
    while not args.empty? and not @stop_parsing
      current = args.pop
      
      if current == "--" then
        while not args.empty?
          yield(:rest, args.pop)
        end
        
      elsif current == "-" then
        yield(:rest, current)

      elsif current[0] == ?- then
        if current[1] == ?- then
          long_option = current[2..-1]
          cmd = @options.find {|item| item.long == long_option }
          
          if not cmd then
            raise CommandLineException, "unrecoginzed option '#{current}'"
          else
            if cmd.argument then
              if args.empty? then
                raise "Error: Option '#{current}' requires argument of type '#{cmd.argument}'"
              else
                yield(cmd.short, args.pop())
              end
            else
              yield(cmd.short, nil)
            end
          end

        else
          # short option
          short_options = current[1..-1]
          
          while not short_options.empty? 
            short = short_options[0]
            short_options = short_options[1..-1]

            cmd = @options.find {|item| item.short == short}
            
            if not cmd then
              raise CommandLineException, "unrecoginzed option '#{current}'"
            else
              if cmd.argument then
                if not short_options.empty? then
                  yield(cmd.short, short_options)
                  short_options = ""
                else
                  if args.empty? then
                    raise CommandLineException, "Error: Option '#{current}' requires argument of type '#{cmd.argument}'"
                  else
                    yield(cmd.short, args.pop())
                  end    
                end
              else
                yield(cmd.short, nil)
              end
            end
          end

        end
      else
        # rest argument
        yield(:rest, current)
      end
    end
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

  def exit()
    @stop_parsing = true
  end
end

cmd = CommandLine.new() {
  name("Windstille Editor V0.1")
  usage("windstille-editor [OPTION]... [FILE]...")
  description("Editor for editing Windstille map files.")
  
  group("Display")
  option(?f, "fullscreen", nil,            "Launch in fullscreen mode")
  option(?g, "geometry",   "WIDTHxHEIGHT", "Launch in the given resolution")

  group("Misc")
  option(?h, "help",       nil,            "Print this help")

  text("If you have throuble launching, try to cleanup ~/.windstille-editor/config.scm" \
       "or contact grumbel@gmx.de")
}

if false then
  begin
    cmd.parse(ARGV) { |option, argument|
      case option
      when ?f
        puts("Fullscreen")

      when ?g
        puts("Geometry: #{argument}")
        
      when ?h
        cmd.print_help()
        cmd.exit()

      when :rest
        puts("Rest: #{argument}")
      end
    }

  rescue CommandLineException => err
    puts('windstille-editor:' + err)
  end
end

# EOF #
