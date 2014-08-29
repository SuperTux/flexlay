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
    result = []
    args = args.reverse()

    @stop_parsing = false
    while not args.empty? and not @stop_parsing
      current = args.pop
      
      if current == "--" then
        while not args.empty?
          result.push([:rest, args.pop])
        end
        
      elsif current == "-" then
        result.push([:rest, current])

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
                result.push([cmd.short, args.pop()])
              end
            else
              result.push([cmd.short, nil])
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
                  result.push([cmd.short, short_options])
                  short_options = ""
                else
                  if args.empty? then
                    raise CommandLineException, "Error: Option '#{current}' requires argument of type '#{cmd.argument}'"
                  else
                    result.push([cmd.short, args.pop()])
                  end    
                end
              else
                result.push([cmd.short, nil])
              end
            end
          end

        end
      else
        # rest argument
        result.push([:rest, current])
      end
    end

    return result
  end

  def print_help()
    puts(@name) if @name
    puts("Usage: #{$0} #{@usage}") if @usage
    puts("")
    puts(@description) if @description

    @help.each() { |item|
      if item.is_a?(String)
        puts(item)
      elsif item.is_a?(CommandLineOption)
        puts("  %-30s %s" % 
               [("%s%s %s" % [if item.short.is_a?(Fixnum) then
                            if item.long then
                              "-#{item.short.chr}, " 
                            else
                              "-#{item.short.chr}" 
                            end
                          else
                            ""
                          end,
                            if item.long then
                              "--#{item.long}"
                            else
                              ""
                            end,
                   if item.argument then item.argument else "" end,
                 ]),
               item.description])
      end
    }
  end

  def exit()
    @stop_parsing = true
  end
end

# EOF #
